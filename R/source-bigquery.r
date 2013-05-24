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
#' library(dplyr)
#' billing <- "341409650721" # put your project number here
#' births <- source_bigquery("publicdata", "samples", "natality", billing)
#' dim(births)
#' colnames(births)
#'
#' head(births)
#'
#' summarise(births, first_year = min(year), last_year = max(year))
#' date_info <- select(births, year:wday)
#' head(date_info)
#'
#' head(filter(select(births, year:wday), year > 2000))
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

is_table <- function(x) {
 is.null(x$select) && is.null(x$filter) && is.null(x$arrange)
}

#' @importFrom dplyr source_vars
#' @S3method source_vars source_bigquery
source_vars.source_bigquery <- function(x) {
  x$select %||% x$vars
}

#' @S3method as.data.frame source_bigquery
as.data.frame.source_bigquery <- function(x, row.names = NULL, optional = NULL,
                                          ..., n = 1e5) {
  if (!is.null(row.names)) warning("row.names argument ignored", call. = FALSE)
  if (!is.null(optional)) warning("optional argument ignored", call. = FALSE)

  get_n_rows(x, n)
}

#' @S3method print source_bigquery
#' @importFrom dplyr dim_desc trunc_mat
print.source_bigquery <- function(x, ...) {
  cat("Source:  Bigquery [", x$project, ":", x$dataset, "/", x$table, "]\n",
    sep = "")
  cat("Table:   ", x$table, " ", dim_desc(x), "\n", sep = "")
  if (!is.null(x$filter)) {
    cat(wrap("Filter:  ", commas(deparse_all(x$filter))), "\n")
  }
  if (!is.null(x$arrange)) {
    cat(wrap("Arrange:  ", commas(deparse_all(x$arrange))), "\n")
  }

  if (is_table(x)) {
    cat("\n")
    trunc_mat(x)
  }
}

#' @S3method dimnames source_bigquery
dimnames.source_bigquery <- function(x) {
  list(NULL, source_vars(x))
}

#' @S3method dim source_bigquery
dim.source_bigquery <- function(x) {
  c(x$n, length(source_vars(x)))
}

#' @S3method head source_bigquery
head.source_bigquery <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  get_n_rows(x, n, warn = FALSE)
}

#' @S3method tail source_bigquery
tail.source_bigquery <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  stop("tail not currently supported for bigquery source", call. = FALSE)
}

# Retrieve data, switching between the (cheap) list_tabledata vs. the
# (expensive) query_exec depending on where or not we actually need to
# run a query
get_n_rows <- function(x, n, warn = TRUE) {
  assert_that(is.numeric(n), length(n) == 1, n > 0)

  if (is_table(x)) {
    if (n > 1e4) {
      page_size <- 1e4
      max_pages <- ceiling(n / 1e4)
    } else {
      page_size <- n
      max_pages <- 1
    }


    list_tabledata(x$project, x$dataset, x$table, warn = warn,
      max_pages = max_pages, page_size = page_size)[seq_len(n), , drop = FALSE]
  } else {
    bq_select(x, n = n, warn = warn)
  }
}


#' @importFrom dplyr is.source select_query
bq_select <- function(x, select = NULL, where = NULL, order_by = NULL, ...,
                       max_pages = 10L, page_size = 1e4L, warn = TRUE,
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

  query_exec(x$project, x$dataset, query = sql, billing = x$billing,
    warn = warn, max_pages = max_pages, page_size = page_size)
}

# Standard manipulation methods -----------------------------------------------

#' @rdname source_bigquery
#' @export
#' @method filter source_bigquery
#' @importFrom dplyr filter
filter.source_bigquery <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  .data$filter <- c(.data$filter, input)
  .data
}

#' @rdname source_bigquery
#' @export
#' @method arrange source_bigquery
#' @importFrom dplyr arrange
arrange.source_bigquery <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  .data$arrange <- c(.data$arrange, input)
  .data
}

#' @rdname source_bigquery
#' @export
#' @method select source_bigquery
#' @importFrom dplyr select
select.source_bigquery <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())
  .data$select <- c(.data$select, input)
  .data
}

#' @rdname source_bigquery
#' @export
#' @method summarise source_bigquery
#' @importFrom dplyr summarise
summarise.source_bigquery <- function(.data, ..., .max_pages = 10L,
                                      .page_size = 1e4L) {
  assert_that(length(.max_pages) == 1, .max_pages > 0L)
  assert_that(length(.page_size) == 1, .page_size > 0L)

  if (!is.null(.data$select)) {
    warning("Summarise ignores selected variables", call. = FALSE)
  }

  select <- trans_bigquery(dots(...), .data, parent.frame())
  out <- bq_select(.data, select = select, max_pages = .max_pages,
    page_size = .page_size)

  source_df(out)
}

#' @rdname source_bigquery
#' @export
#' @method mutate source_bigquery
#' @importFrom dplyr mutate
mutate.source_bigquery <- function(.data, ..., .max_pages = 10L,
                                   .page_size = 1e4L) {
  assert_that(length(.max_pages) == 1, .max_pages > 0L)
  assert_that(length(.page_size) == 1, .page_size > 0L)

  old_vars <- .data$select %||% .data$vars
  new_vars <- trans_bigquery(dots(...), .data, parent.frame())

  out <- bq_select(.data, select = c(old_vars, new_vars), n = .n)
  source_df(out)
}
