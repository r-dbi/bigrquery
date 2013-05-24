#' A grouped bigquery database.
#'
#' Typically you will create a grouped data table is to call the \code{group_by}
#' method on a bigquery data source: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @param source a data source or data frame.
#' @param group_by \code{vars} partially evaluated in the correct environment
#' @export
#' @examples
#' billing <- "341409650721" # put your project number here
#' births <- source_bigquery("publicdata", "samples", "natality", billing)
#' by_year_sex <- group_by(births, year, is_male)
#' wt <- summarise(by_year_sex, n = count(), wt = mean(weight_pounds))
#' wtdf <- as.data.frame(wt)
#' if (require("ggplot2")) {
#' qplot(year, wt, data = wtdf, geom = "line", colour = is_male)
#' }
grouped_bigquery <- function(source, group_by) {
  source$group_by <- group_by
  class(source) <- c("grouped_bigquery", class(source))

  source
}

#' @export
#' @rdname grouped_bigquery
#' @method group_by source_bigquery
#' @param x an existing bigquery data source
#' @param ... expressions describing how to group data
#' @importFrom dplyr group_by
group_by.source_bigquery <- function(x, ...) {
  group_by <- partial_eval(named_dots(...), x, parent.frame())
  grouped_bigquery(x, group_by)
}

#' @S3method print grouped_bigquery
print.grouped_bigquery <- function(x, ...) {
  cat("Source: bigquery [", x$path, "]\n", sep = "")
  cat("Table:  ", x$table, dim_desc(x), "\n", sep = "")
  cat("Groups: ", paste0(deparse_all(x$vars), collapse = ", "), "\n", sep = "")

  if (is_table(x)) {
    cat("\n")
    trunc_mat(x)
  }
}

#' @rdname grouped_bigquery
#' @export
#' @method filter grouped_bigquery
filter.grouped_bigquery <- function(.data, ...) {
  warning("Group by ignored for filtering with bigquery", call. = FALSE)

  filter.source_bigquery(.data, ...)
}

#' @rdname grouped_bigquery
#' @export
#' @method summarise grouped_bigquery
summarise.grouped_bigquery <- function(.data, ..., .max_pages = 10L,
                                       .page_size = 1e4L) {
  assert_that(length(.max_pages) == 1, .max_pages > 0L)
  assert_that(length(.page_size) == 1, .page_size > 0L)

  select <- trans_bigquery(dots(...), .data, parent.frame())
  group_by <- trans_bigquery(.data$group_by)

  out <- bq_select(.data,
    select = c(group_by, select),
    group_by = names(group_by),
    max_pages = .max_pages,
    page_size = .page_size)

  source_df(out)
}

#' @rdname grouped_bigquery
#' @export
#' @method mutate grouped_bigquery
mutate.grouped_bigquery <- function(.data, ..., .n = 1e5) {
  assert_that(length(.n) == 1, .n > 0L)

  warning("Group by ignored for mutate with bigquery", call. = FALSE)

  out <- mutate.source_bigquery(.data, ..., .n = .n)
  grouped_df(
    data = out,
    vars = .data$vars
  )
}

#' @rdname grouped_bigquery
#' @export
#' @method arrange grouped_bigquery
arrange.grouped_bigquery <- function(.data, ...) {
  warning("Group by ignored for arrange with bigquery", call. = FALSE)

  arrange.source_bigquery(.data, ...)
}

#' @rdname grouped_bigquery
#' @export
#' @method select grouped_bigquery
select.grouped_bigquery <- function(.data, ...) {
  warning("Group by ignored for select with bigquery", call. = FALSE)

  select.source_bigquery(.data, ...)
}
