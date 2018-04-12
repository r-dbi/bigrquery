#' A bigquery data source.
#'
#' Use `src_bigquery` to connect to an existing bigquery dataset,
#' and `tbl` to connect to tables within that database.
#'
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to `project`
#' @param max_pages (IGNORED) max pages returned by a query
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To run this example, replace billing with the id of one of your projects
#' # set up for billing
#' con <- DBI::dbConnect(dbi_driver(),
#'   project = "publicdata",
#'   dataset = "samples",
#'   billing = "887175176791"
#' )
#'
#' DBI::dbListTables(con)
#' DBI::dbGetQuery(con, "SELECT * FROM gsod LIMIT 5")
#'
#' # You can also use the dplyr interface
#' shakespeare <- con %>% tbl("shakespeare")
#' shakespeare
#' shakespeare %>%
#'   group_by(word) %>%
#'   summarise(n = sum(word_count)) %>%
#'   arrange(desc(n))
#' }
src_bigquery <- function(project, dataset, billing = project, max_pages = 10) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required to use src_bigquery", call. = FALSE)
  }
  if (!use_dbplyr()) {
    stop("dplyr 0.6.0 and dbplyr required to use src_bigquery", call. = FALSE)
  }

  con <- DBI::dbConnect(
    dbi_driver(),
    project = project,
    dataset = dataset,
    billing = billing,
    use_legacy_sql = FALSE
  )

  dbplyr::src_dbi(con)
}

# registered onLoad
tbl.src_bigquery <- function(src, from, ...) {
  if (src@use_legacy_sql) {
    stop("dplyr backend must have use_legacy_sql = FALSE", call. = FALSE)
  }
  dbplyr::tbl_sql("bigquery", src = src, from = from, ...)
}

# registered onLoad
db_query_fields.BigQueryConnection <- function(con, sql) {
  if (dbplyr::is.sql(sql)) {
    ds <- bq_dataset(con@project, con@dataset)
    fields <- bq_query_fields(sql, con@billing, default_dataset = ds)
  } else {
    tb <- bq_table(con@project, con@dataset, sql)
    fields <- bq_table_fields(tb)
  }

  vapply(fields, "[[", "name", FUN.VALUE = character(1))
}

# SQL translation -------------------------------------------------------------
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
      str_detect =  sql_prefix("REGEXP_MATCH", 2),
      str_extract = sql_prefix("REGEXP_EXTRACT", 2),
      str_replace = sql_prefix("REGEXP_REPLACE", 3),

      # Parallel min and max
      pmax = sql_prefix("GREATEST"),
      pmin = sql_prefix("LEAST")
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
