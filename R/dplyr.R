#' A BigQuery data source for dplyr.
#'
#' Create the connection to the database with `DBI::dbConnect()` then
#' use [dplyr::tbl()] to connect to tables within that database. Generally,
#' it's best to provide the fully qualified name of the table (i.e.
#' `project.dataset.table`) but if you supply a default `dataset` in the
#' connection, you can use just the table name. (This, however, will
#' prevent you from making joins across datasets.)
#'
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to `project`
#' @param max_pages (IGNORED) maximum pages returned by a query
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To run this example, replace billing with the id of one of your projects
#' # set up for billing
#' con <- DBI::dbConnect(bigquery(), project = bq_test_project())
#'
#' shakespeare <- con %>% tbl("publicdata.samples.shakespeare")
#' shakespeare
#' shakespeare %>%
#'   group_by(word) %>%
#'   summarise(n = sum(word_count, na.rm = TRUE)) %>%
#'   arrange(desc(n))
#' }
src_bigquery <- function(project, dataset, billing = project, max_pages = 10) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required to use src_bigquery", call. = FALSE)
  }

  con <- DBI::dbConnect(
    bigquery(),
    project = project,
    dataset = dataset,
    billing = billing,
    use_legacy_sql = FALSE
  )

  dbplyr::src_dbi(con)
}

# registered onLoad
db_query_fields.BigQueryConnection <- function(con, sql) {
  if (dbplyr::is.sql(sql)) {
    ds <- bq_dataset(con@project, con@dataset)
    fields <- bq_query_fields(sql, con@billing, default_dataset = ds)
  } else {
    tb <- as_bq_table(con, sql)
    fields <- bq_table_fields(tb)
  }

  vapply(fields, "[[", "name", FUN.VALUE = character(1))
}

# registered onLoad
db_save_query.BigQueryConnection <- function(con, sql, name, temporary = TRUE, ...) {

  if (is.null(con@dataset)) {
    destination_table <- if (!temporary) as_bq_table(con, name)
    tb <- bq_project_query(con@project, sql, destination_table = destination_table)
  } else {
    ds <- bq_dataset(con@project, con@dataset)
    destination_table <- if (!temporary) as_bq_table(con, name)

    tb <- bq_dataset_query(ds,
      query = sql,
      destination_table = destination_table
    )
  }

  paste0(tb$project, ".", tb$dataset, ".", tb$table)
}

# registered onLoad
db_analyze.BigQueryConnection <- function(con, table, ...) {
  TRUE
}

# registered onLoad
db_copy_to.BigQueryConnection <- function(con, table, values,
                            overwrite = FALSE, types = NULL, temporary = TRUE,
                            unique_indexes = NULL, indexes = NULL,
                            analyze = TRUE, ...) {

  if (temporary) {
    rlang::abort("BigQuery does not support temporary tables")
  }

  tb <- bq_table(con@project, con@dataset, table)
  write <- if (overwrite) "WRITE_TRUNCATE" else "WRITE_EMPTY"
  bq_table_upload(tb, values, fields = types, write_disposition = write)

  table
}

# Efficient downloads -----------------------------------------------

# registered onLoad
collect.tbl_BigQueryConnection <- function(x, ...,
                                           page_size = 1e4,
                                           max_connections = 6L,
                                           n = Inf,
                                           warn_incomplete = TRUE) {
  assert_that(length(n) == 1, n > 0L)

  if (op_can_download(x$ops)) {
    name <- op_table(x$ops, x$src$con)
    tb <- as_bq_table(x$src$con, name)
    n <- min(op_rows(x$ops), n)
  } else {
    sql <- dbplyr::db_sql_render(x$src$con, x)

    billing <- x$src$con@billing
    if (is.null(x$src$con@dataset)) {
      tb <- bq_project_query(billing, sql, quiet = x$src$con@quiet, ...)
    } else {
      ds <- as_bq_dataset(x$src$con)
      tb <- bq_dataset_query(ds, sql, quiet = x$src$con@quiet, billing = billing, ...)
    }
  }

  quiet <- if (n < 100) TRUE else x$src$con@quiet
  out <- bq_table_download(tb,
    max_results = n,
    page_size = page_size,
    quiet = quiet,
    max_connections = max_connections
  )
  dplyr::grouped_df(out, intersect(dbplyr::op_grps(x), names(out)))
}

# Can download directly if only head and

op_can_download <- function(x) UseMethod("op_can_download")
#' @export
op_can_download.op <- function(x) FALSE
#' @export
op_can_download.op_head <- function(x) op_can_download(x$x)
#' @export
op_can_download.op_base_remote <- function(x) dbplyr::is.ident(x$x)

op_rows <- function(x) UseMethod("op_rows")
#' @export
op_rows.op_base <- function(x) Inf
#' @export
op_rows.op_head <- function(x) min(x$args$n, op_rows(x$x))

op_table <- function(x, con) UseMethod("op_table")
#' @export
op_table.op <- function(x, con) op_table(x$x, con)
#' @export
op_table.op_base_remote <- function(x, con) {
  x$x
}

# SQL translation -------------------------------------------------------------

# Don't import to avoid build-time dependency
sql_prefix <- function(f, n = NULL) {
  dbplyr::sql_prefix(f = f, n = n)
}

# registered onLoad
sql_join_suffix.BigQueryConnection <- function(con, ...) {
  c("_x", "_y")
}

# registered onLoad
sql_translate_env.BigQueryConnection <- function(x) {
  dbplyr::sql_variant(
    dbplyr::sql_translator(.parent = dbplyr::base_scalar,

      `^` = sql_prefix("POW"),
      `%%` = sql_prefix("MOD"),
      "%||%" = sql_prefix("IFNULL"),

      # Coercion
      as.integer = function(x) dbplyr::build_sql("SAFE_CAST(", x, " AS INT64)"),
      as.logical = function(x) dbplyr::build_sql("SAFE_CAST(", x, " AS BOOLEAN)"),
      as.numeric = function(x) dbplyr::build_sql("SAFE_CAST(", x, " AS FLOAT64)"),
      as.character = function(x) dbplyr::build_sql("SAFE_CAST(", x, " AS STRING)"),

      # Date/time
      Sys.date = sql_prefix("current_date"),
      Sys.time = sql_prefix("current_time"),

      # Regular expressions
      grepl = sql_prefix("REGEXP_CONTAINS", 2),
      gsub = function(match, replace, x) {
        dbplyr::build_sql("REGEXP_REPLACE", list(x, match, replace))
      },

      # Other scalar functions
      ifelse = sql_prefix("IF"),

      # string
      paste0 = sql_prefix("CONCAT"),

      # stringr equivalents
      str_detect =  sql_prefix("REGEXP_CONTAINS", 2),
      str_extract = sql_prefix("REGEXP_EXTRACT", 2),
      str_replace = sql_prefix("REGEXP_REPLACE", 3),

      # Parallel min and max
      pmax = sql_prefix("GREATEST"),
      pmin = sql_prefix("LEAST"),

      # Median
      median = function(x) dbplyr::build_sql("APPROX_QUANTILES(", x, ", 2)[SAFE_ORDINAL(2)]")
    ),
    dbplyr::sql_translator(.parent = dbplyr::base_agg,
      n = function() dplyr::sql("count(*)"),

      all = sql_prefix("LOGICAL_AND", 1),
      any = sql_prefix("LOGICAL_OR", 1),

      sd =  sql_prefix("STDDEV_SAMP"),
      var = sql_prefix("VAR_SAMP"),
      cor = dbplyr::sql_aggregate_2("CORR"),
      cov = dbplyr::sql_aggregate_2("COVAR_SAMP")
    ),
    dbplyr::sql_translator(.parent = dbplyr::base_win,
      all = dbplyr::win_absent("LOGICAL_AND"),
      any = dbplyr::win_absent("LOGICAL_OR"),

      sd =  dbplyr::win_recycled("STDDEV_SAMP"),
      var = dbplyr::win_recycled("VAR_SAMP"),
      cor = dbplyr::win_absent("CORR"),
      cov = dbplyr::win_absent("COVAR_SAMP"),

      n_distinct = dbplyr::win_absent("n_distinct")
    )
  )
}

simulate_bigrquery <- function(use_legacy_sql = FALSE) {
  new("BigQueryConnection",
    project = "test",
    dataset = "test",
    billing = "test",
    use_legacy_sql = use_legacy_sql,
    page_size = 0L,
    quiet = TRUE
  )
}
