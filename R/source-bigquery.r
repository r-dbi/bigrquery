#' A bigquery data source.
#'
#' @section Caching:
#' The variable names and number of rows are cached on source creation,
#' on the assumption that you're probably doing analysis on a table that's
#' not changing as you run queries. If this is not correct, then the values
#' of \code{dim} and \code{dimnames} may be out of date, but it shouldn't
#' otherwise affect operation.
#'
#' @examples
#' billing <- "341409650721" # put your project number here
#' births <- source_bigquery("publicdata", "samples", "natality", billing)
#' dim(births)
#' colnames(births)
#'
#' head(births)
source_bigquery <- function(project, dataset, table, billing = project) {
  assert_that(is.string(project), is.string(dataset), is.string(table),
    is.string(billing))

  # Cache variable names and dataset size
  table_data <- get_table(project, dataset, table)
  vars <- vapply(table_data$schema$fields, function(x) x$name, character(1))
  p <- length(vars)
  n <- as.numeric(table_data$numRows)

  structure(list(
      project = project, dataset = dataset, table = table, billing = billing,
      vars = vars, n = n, p = p,
      select = NULL, filter = NULL, arrange = NULL
    ),
    class = c("source_bigquery", "source", "op")
  )
}

#' @importFrom dplyr source_vars
#' @S3method source_vars source_bigquery
source_vars.source_bigquery <- function(x) {
  x$vars
}

#' @S3method as.data.frame source_bigquery
as.data.frame.source_bigquery <- function(x, row.names = NULL, optional = NULL,
                                          ..., max_pages = 10L,
                                          page_size = 1e4L) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  bq_select(x, n = n)
}

#' @S3method print source_bigquery
print.source_bigquery <- function(x, ...) {
  cat("Source:  Bigquery [", x$path, "]\n", sep = "")
  cat("Table:   ", x$table, dim_desc(x), "\n", sep = "")
  if (!is.null(x$filter)) {
    cat(wrap("Filter:  ", commas(deparse_all(x$filter))), "\n")
  }
  if (!is.null(x$arrange)) {
    cat(wrap("Arrange:  ", commas(deparse_all(x$arrange))), "\n")
  }
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames source_bigquery
dimnames.source_bigquery <- function(x) {
  list(NULL, x$vars)
}

#' @S3method dim source_bigquery
dim.source_bigquery <- function(x) {
  c(x$n, x$p)
}

#' @S3method head source_bigquery
head.source_bigquery <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  bq_select(x, limit = n)
}

#' @S3method tail source_bigquery
tail.source_bigquery <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  stop("tail not currently supported for bigquery source", call. = FALSE)
}


#' @importFrom dplyr is.source select_query
bq_select <- function(x, select = NULL, where = NULL, order_by = NULL, ...,
                       max_pages = 10L, page_size = 1e4L,
                       show = getOption("dplyr.show_sql", FALSE)) {
  assert_that(is.source(x))
  assert_that(is.numeric(max_pages), length(max_pages) == 1)
  assert_that(is.numeric(page_size), length(page_size) == 1)
  assert_that(is.flag(show))

  select <- select %||% x$select %||% x$vars
  where <- where %||% trans_bigquery(x$filter)
  order_by <- order_by %||% trans_bigquery(x$arrange)

  sql <- select_query(
    from = x$table,
    select = select,
    where = where,
    order_by = order_by,
    ...)

  if (isTRUE(show)) {
    message(sql)
    cat("\n")
  }

  query_exec(x$project, x$dataset, query = sql,
    billing = x$billing, max_pages = max_pages, page_size = page_size)
}
