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
  check_installed("dbplyr")

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
tbl.BigQueryConnection <- function(src, from, ...) {
  src <- dbplyr::src_dbi(src, auto_disconnect = FALSE)
  tbl <- dplyr::tbl(src, from = from, check_from = FALSE)

  # This is ugly, but I don't see a better way of doing this
  tb <- as_bq_table(src$con, from)
  tbl$lazy_query$is_view <- !inherits(from, "sql") &&
    tryCatch(bq_table_meta(tb, "type")$type == "VIEW", bigrquery_notFound = function(e) TRUE)
  tbl
}

# registered onLoad
dbplyr_edition.BigQueryConnection <- function(con) 2L

# registered onLoad
db_compute.BigQueryConnection <- function(con,
                                          table,
                                          sql,
                                          ...,
                                          overwrite = FALSE,
                                          temporary = TRUE,
                                          unique_indexes = list(),
                                          indexes = list(),
                                          analyze = TRUE,
                                          in_transaction = FALSE) {

  if (is.null(con@dataset)) {
    destination_table <- if (!temporary) as_bq_table(con, table)
    tb <- bq_project_query(con@project, sql, destination_table = destination_table)
  } else {
    ds <- bq_dataset(con@project, con@dataset)
    destination_table <- if (!temporary) as_bq_table(con, table)

    tb <- bq_dataset_query(ds,
      query = sql,
      destination_table = destination_table
    )
  }

  dbplyr::in_catalog(tb$project, tb$dataset, tb$table)
}

# registered onLoad
db_copy_to.BigQueryConnection <- function(con,
                                          table,
                                          values,
                                          ...,
                                          overwrite = FALSE,
                                          types = NULL,
                                          temporary = TRUE,
                                          unique_indexes = NULL,
                                          indexes = NULL,
                                          analyze = TRUE,
                                          in_transaction = TRUE) {

  if (temporary) {
    cli::cli_abort("BigQuery does not support temporary tables")
  }

  tb <- as_bq_table(con, table)
  write <- if (overwrite) "WRITE_TRUNCATE" else "WRITE_EMPTY"
  bq_table_upload(tb, values, fields = types, write_disposition = write)

  table
}

# Efficient downloads -----------------------------------------------

# registered onLoad
collect.tbl_BigQueryConnection <- function(x, ...,
                                           page_size = NULL,
                                           max_connections = 6L,
                                           n = Inf,
                                           warn_incomplete = TRUE) {

  check_number_whole(n, min = 0, allow_infinite = TRUE)
  check_number_whole(max_connections, min = 1)
  check_bool(warn_incomplete)

  con <- dbplyr::remote_con(x)

  if (op_can_download(x)) {
    lq <- x$lazy_query
    name <- op_table(x, con)
    tb <- as_bq_table(con, name)
    n <- min(op_rows(x$lazy_query), n)
  } else {
    sql <- dbplyr::db_sql_render(con, x)

    billing <- con@billing
    if (is.null(con@dataset)) {
      tb <- bq_project_query(billing, sql, quiet = con@quiet, ...)
    } else {
      ds <- as_bq_dataset(con)
      tb <- bq_dataset_query(ds, sql, quiet = con@quiet, billing = billing, ...)
    }
  }

  quiet <- if (n < 100) TRUE else con@quiet
  bigint <- con@bigint %||% "integer"
  out <- bq_table_download(tb,
    n_max = n,
    page_size = page_size,
    quiet = quiet,
    max_connections = max_connections,
    bigint = bigint
  )
  dplyr::grouped_df(out, intersect(dbplyr::op_grps(x), names(out)))
}

# Can download directly if only head and

op_can_download <- function(x) UseMethod("op_can_download")
#' @export
op_can_download.tbl_lazy <- function(x) op_can_download(x$lazy_query)
#' @export
op_can_download.lazy_query <- function(x) FALSE
#' @export
op_can_download.lazy_select_query <- function(x) {
  query_is_head_only(x)
}
#' @export
op_can_download.lazy_base_query <- function(x) {
  if (isTRUE(x$is_view)) {
    FALSE
  } else if (inherits(x$x, "sql")) {
    FALSE
  } else {
    dbplyr::is.ident(x$x) || inherits(x$x, "dbplyr_table_ident")
  }
}

query_is_head_only <- function(x) {
  if (!inherits(x$x, "lazy_base_remote_query")) return(FALSE)
  if (!op_can_download(x$x)) return(FALSE)

  vars_base <- dbplyr::op_vars(x$x)
  if (!is_select_trivial(x$select, vars_base)) return(FALSE)

  if (!is_empty(x$where)) return(FALSE)
  if (!is_empty(x$order_by)) return(FALSE)
  if (!is_false(x$distinct)) return(FALSE)

  TRUE
}

is_select_trivial <- function(select, vars_prev) {
  identical(select$name, vars_prev) &&
    all(vapply(select$expr, is_symbol, logical(1))) &&
    identical(syms(select$name), select$expr)
}


op_rows <- function(x) UseMethod("op_rows")
#' @export
op_rows.tbl_lazy <- function(x) op_rows(x$lazy_query)
#' @export
op_rows.lazy_base_query <- function(x) Inf
#' @export
op_rows.lazy_select_query <- function(x) {
  min(x$limit, op_rows(x$x))
}

op_table <- function(x, con) UseMethod("op_table")
#' @export
op_table.tbl_lazy <- function(x, con) op_table(x$lazy_query, con)
#' @export
op_table.lazy_query <- function(x, con) NULL
#' @export
op_table.lazy_base_remote_query <- function(x, con) x$x
#' @export
op_table.lazy_select_query <- function(x, con) {
  if (!query_is_head_only(x)) return(NULL)

  op_table(x$x)
}


# exported onLoad
same_src.tbl_BigQueryConnection <- function(x, y) {
  inherits(y, "tbl_BigQueryConnection")
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
sql_translation.BigQueryConnection <- function(x) {
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
      grepl = function(pattern, x) {
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/string_functions#regexp_contains
        dbplyr::build_sql("REGEXP_CONTAINS", list(x, pattern))
      },
      gsub = function(pattern, replace, x) {
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/string_functions#regexp_replace
        dbplyr::build_sql("REGEXP_REPLACE", list(x, pattern, replace))
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

      runif = function(n = n(), min = 0, max = 1) {
        RAND <- NULL # quiet R CMD check
        dbplyr::sql_runif(RAND(), n = {{ n }}, min = min, max = max)
      },
    ),
    dbplyr::sql_translator(.parent = dbplyr::base_agg,
      n = function() dplyr::sql("count(*)"),

      all = sql_prefix("LOGICAL_AND", 1),
      any = sql_prefix("LOGICAL_OR", 1),

      sd =  sql_prefix("STDDEV_SAMP"),
      var = sql_prefix("VAR_SAMP"),
      cor = dbplyr::sql_aggregate_2("CORR"),
      cov = dbplyr::sql_aggregate_2("COVAR_SAMP"),

      # Median
      median = function(x, na.rm = TRUE) {
        dbplyr::build_sql("APPROX_QUANTILES(", x, ", 2)[SAFE_ORDINAL(2)]")
      }
    ),
    dbplyr::sql_translator(.parent = dbplyr::base_win,
      all = dbplyr::win_absent("LOGICAL_AND"),
      any = dbplyr::win_absent("LOGICAL_OR"),

      sd =  dbplyr::win_recycled("STDDEV_SAMP"),
      var = dbplyr::win_recycled("VAR_SAMP"),
      cor = dbplyr::win_absent("CORR"),
      cov = dbplyr::win_absent("COVAR_SAMP"),

      n_distinct = dbplyr::win_absent("n_distinct"),

      median = dbplyr::win_absent("median")
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
