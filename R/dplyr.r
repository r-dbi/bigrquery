#' A bigquery data source.
#'
#' Use \code{src_bigquery} to connect to an existing bigquery dataset,
#' and \code{tbl} to connect to tables within that database.
#'
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to \code{project}
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To run this example, replace billing with the id of one of your projects
#' # set up for billing
#' pd <- src_bigquery("publicdata", "samples", billing = "465736758727")
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
#'   collect()
#' plot(year_weights$year, year_weights$weight, type = "b")
#' }
src_bigquery <- function(project, dataset, billing = project) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required to use src_bigquery", call. = FALSE)
  }

  assert_that(is.string(project), is.string(dataset), is.string(billing))

  if (!require("bigrquery")) {
    stop("bigrquery package required to connect to bigquery db", call. = FALSE)
  }

  con <- structure(list(project = project, dataset = dataset,
    billing = billing), class = "bigquery")
  dplyr::src_sql("bigquery", con)
}

#' @export
#' @importFrom dplyr tbl
tbl.src_bigquery <- function(src, from, ...) {
  dplyr::tbl_sql("bigquery", src = src, from = from, ...)
}

#' @export
#' @importFrom dplyr copy_to
copy_to.src_bigquery <- function(dest, df, name = deparse(substitute(df)), ...) {
  job <- insert_upload_job(dest$con$project, dest$con$dataset, name, df,
    billing = dest$con$billing)
  wait_for(job)

  tbl(dest, name)
}

#' @export
#' @importFrom dplyr src_desc
src_desc.src_bigquery <- function(x) {
  paste0("bigquery [", format_dataset(x$con$project, x$con$dataset), "]")
}

#' @export
#' @importFrom dplyr db_list_tables
db_list_tables.bigquery <- function(con) {
  list_tables(con$project, con$dataset)
}

#' @export
#' @importFrom dplyr db_has_table
db_has_table.bigquery <- function(con, table) {
  table %in% list_tables(con$project, con$dataset)
}

#' @export
#' @importFrom dplyr db_query_fields
db_query_fields.bigquery <- function(con, sql) {
  info <- get_table(con$project, con$dataset, sql)
  vapply(info$schema$fields, "[[", "name", FUN.VALUE = character(1))
}

#' @export
#' @importFrom dplyr db_query_rows
db_query_rows.bigquery <- function(con, sql) {
  browser()
  info <- get_table(con$project, con$dataset, sql)
  browser()
}

#' @export
dim.tbl_bigquery <- function(x) {
  p <- x$query$ncol()
  c(NA, p)
}

#' @export
#' @importFrom dplyr mutate_
mutate_.tbl_bigquery <- function(...) {

  # BigQuery requires a collapse after any mutate
  dplyr::collapse( dplyr:::mutate_.tbl_sql(...) )
}

# SQL -------------------------------------------------------------------------

#' @export
#' @importFrom dplyr sql_select
sql_select.bigquery <- function(con, select, from, where = NULL,
                                group_by = NULL, having = NULL,
                                order_by = NULL, limit = NULL,
                                offset = NULL, ...) {

  dplyr:::sql_select.DBIConnection(con, select, from, where = where,
    group_by = group_by, having = having, order_by = order_by,
    limit = limit, offset = offset, ...)
}

#' @export
#' @importFrom dplyr sql_subquery
sql_subquery.bigquery <- function(con, sql, name =  dplyr::unique_name(), ...) {
  if (dplyr::is.ident(sql)) return(sql)

  dplyr::build_sql("(", sql, ") AS ", dplyr::ident(name), con = con)
}


#' @export
#' @importFrom dplyr query
query.bigquery <- function(con, sql, .vars) {
  assert_that(is.string(sql))

  BigQuery$new(con, sql(sql), .vars)
}

#' @importFrom R6 R6Class
BigQuery <- R6::R6Class("BigQuery",
  private = list(
    .nrow = NULL,
    .vars = NULL
  ),
  public = list(
    con = NULL,
    sql = NULL,

    initialize = function(con, sql, vars) {
      self$con <- con
      self$sql <- sql
      private$.vars <- vars
    },

    print = function(...) {
      cat("<Query> ", self$sql, "\n", sep = "")
      print(self$con)
    },

    fetch = function(n = -1L) {
      job <- insert_query_job(self$sql, self$con$billing,
        default_dataset = format_dataset(self$con$project, self$con$dataset))
      job <- wait_for(job)

      dest <- job$configuration$query$destinationTable
      list_tabledata(dest$projectId, dest$datasetId, dest$tableId)
    },

    fetch_paged = function(chunk_size = 1e4, callback) {
      job <- insert_query_job(self$sql, self$con$billing,
        default_dataset = format_dataset(self$con$project, self$con$dataset))
      job <- wait_for(job)

      dest <- job$configuration$query$destinationTable
      list_tabledata_callback(dest$projectId, dest$datasetId, dest$tableId,
        callback, page_size = chunk_size)
    },

    vars = function() {
      private$.vars
    },

    nrow = function() {
      if (!is.null(private$.nrow)) return(private$.nrow)
      private$.nrow <- db_query_rows(self$con, self$sql)
      private$.nrow
    },

    ncol = function() {
      length(self$vars())
    }
  )
)


# SQL translation --------------------------------------------------------------

#' @export
#' @importFrom dplyr src_translate_env
src_translate_env.src_bigquery <- function(x) {
  dplyr::sql_variant(
    dplyr::sql_translator(.parent = dplyr::base_scalar,
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
sql_escape_string.bigquery <- function(con, x) {
  encodeString(x, na.encode = FALSE, quote = '"')
}

#' @export
#' @importFrom dplyr sql_escape_ident
sql_escape_ident.bigquery <- function(con, x) {
  y <- paste0("[", x, "]")
  y[is.na(x)] <- "NULL"
  names(y) <- names(x)

  y
}
