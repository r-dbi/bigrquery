#' A bigquery data source.
#'
#' Use \code{src_bigquery} to connect to an existing bigquery dataset,
#' and \code{tbl} to connect to tables within that database.
#'
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to \code{project}
#' @param max_pages (IGNORED) max pages returned by a query
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To run this example, replace billing with the id of one of your projects
#' # set up for billing
#' pd <- src_bigquery("publicdata", "samples", billing = "887175176791")
#' pd %>% tbl("shakespeare")
#'
#' # With bigquery data, it's always a good idea to start by selecting
#' # only the variables you're interested in - this reduces the amount of
#' # data that needs to be scanned and hence decreases costs
#' natality <- pd %>%
#'   tbl("natality") %>%
#'   select(year:day, state, child_race, weight_pounds)
#' year_weights <- natality %>%
#'   group_by(year) %>%
#'   summarise(weight = mean(weight_pounds), n = n()) %>%
#'   arrange(year) %>%
#'   collect()
#' plot(year_weights$year, year_weights$weight, type = "b")
#' }
src_bigquery <- function(project, dataset, billing = project, max_pages = 10) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required to use src_bigquery", call. = FALSE)
  }

  assert_that(is.string(project), is.string(dataset), is.string(billing))

  con <- DBI::dbConnect(
    dbi_driver(),
    project = project,
    dataset = dataset,
    billing = billing
  )
  dplyr::src_sql("bigquery", con)
}

#' @export
#' @importFrom dplyr tbl
tbl.src_bigquery <- function(src, from, ...) {
  dplyr::tbl_sql("bigquery", src = src, from = from, ...)
}

#' @export
#' @importFrom dplyr src_desc
src_desc.src_bigquery <- function(x) {
  paste0("bigquery [", format_dataset(x$con@project, x$con@dataset), "]")
}

#' @export
#' @importFrom dplyr db_query_fields
db_query_fields.BigQueryConnection <- function(con, sql) {
  info <- get_table(con@project, con@dataset, sql)
  vapply(info$schema$fields, "[[", "name", FUN.VALUE = character(1))
}

# SQL translation -------------------------------------------------------------
#' @importFrom dplyr sql_translate_env
#' @export
sql_translate_env.BigQueryConnection <- function(x) {
  dplyr::sql_variant(
    dplyr::sql_translator(.parent = dplyr::base_scalar,
      "^" = dplyr::sql_prefix("pow"),

      # Casting
      as.logical = dplyr::sql_prefix("boolean"),
      as.numeric = dplyr::sql_prefix("float"),
      as.double = dplyr::sql_prefix("float"),
      as.integer = dplyr::sql_prefix("integer"),
      as.character = dplyr::sql_prefix("string"),

      # Date/time
      Sys.date = dplyr::sql_prefix("current_date"),
      Sys.time = dplyr::sql_prefix("current_time"),

      # Regular expressions
      grepl = function(match, x) {
        sprintf("REGEXP_MATCH(%s, %s)", dplyr::escape(x), dplyr::escape(match))
      },
      gsub = function(match, replace, x) {
        sprintf("REGEXP_REPLACE(%s, %s, %s)", dplyr::escape(x),
          dplyr::escape(match), dplyr::escape(replace))
      },

      # Other scalar functions
      ifelse = dplyr::sql_prefix("IF"),

      # stringr equivalents
      str_detect = function(x, match) {
        sprintf("REGEXP_MATCH(%s, %s)", dplyr::escape(x),
          dplyr::escape(match))
      },
      str_extract = function(x, match) {
        sprintf("REGEXP_EXTRACT(%s, %s)",  dplyr::escape(x),
          dplyr::escape(match))
      },
      str_replace = function(x, match, replace) {
        sprintf("REGEXP_REPLACE(%s, %s, %s)", dplyr::escape(x),
          dplyr::escape(match), dplyr::escape(replace))
      }
    ),
    dplyr::sql_translator(.parent = dplyr::base_agg,
      n = function() dplyr::sql("count(*)"),
      "%||%" = dplyr::sql_prefix("concat"),
      sd =  dplyr::sql_prefix("stddev")
    ),
    dplyr::sql_translator(.parent = dplyr::base_win,
      mean  = function(...) stop("Not supported by bigquery"),
      sum   = function(...) stop("Not supported by bigquery"),
      min   = function(...) stop("Not supported by bigquery"),
      max   = function(...) stop("Not supported by bigquery"),
      n     = function(...) stop("Not supported by bigquery"),
      cummean = win_bq("mean"),
      cumsum  = win_bq("sum"),
      cummin  = win_bq("min"),
      cummax  = win_bq("max")
    )
  )
}

# BQ doesn't need frame clause
win_bq <- function(f) {
  force(f)
  function(x) {
    dplyr:::over(
      dplyr::build_sql(dplyr::sql(f), list(x)),
      dplyr:::partition_group(),
      dplyr:::partition_order()
    )
  }
}

#' @export
#' @importFrom dplyr sql_escape_string
sql_escape_string.BigQueryConnection <- function(con, x) {
  encodeString(x, na.encode = FALSE, quote = '"')
}

#' @export
#' @importFrom dplyr sql_escape_ident
sql_escape_ident.BigQueryConnection <- function(con, x) {
  y <- paste0("[", x, "]")
  y[is.na(x)] <- "NULL"
  names(y) <- names(x)

  y
}
