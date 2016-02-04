#' Retrieve data from a table.
#'
#' \code{list_tabledata} returns a single dataframe.
#' \code{list_tabledata_callback} calls the supplied callback with each page
#' of data.
#'
#' @inheritParams get_table
#' @param callback function called with single argument, the data from the
#'   current page of data
#' @param quiet if \code{FALSE}, prints informative status messages.
#' @param table_info if known, the table information retrieved with
#'   \code{\link{get_table}}
#' @param page_size Number of items per page.
#' @param warn If \code{TRUE}, warn when there are rows remaining to
#'   be pulled down from database.
#' @param max_pages maximum number of pages to retrieve. Use \code{Inf}
#'  to retrieve the complete dataset.
#' @seealso API documentation at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/tabledata/list}
#' @export
#' @examples
#' \dontrun{
#' billing_project <- "341409650721" # put your project number here
#' natal <- list_tabledata("publicdata", "samples", "natality", max_pages = 2,
#'   page_size = 10)
#' dim(natal)
#' }
list_tabledata <- function(project, dataset, table, page_size = 1e4,
                           table_info = NULL, max_pages = 10, warn = TRUE,
                           quiet = getOption("bigquery.quiet")) {
  assert_that(is.string(project), is.string(dataset), is.string(table))
  assert_that(is.numeric(max_pages), length(max_pages) == 1, max_pages >= 1)

  # This is a rather inefficient implementation - better strategy would be
  # preallocate list when max_pages is finite, and use doubling strategy
  # when it's not.
  rows <- list()
  append_rows <- function(new_rows) {
    rows <<- c(rows, list(new_rows))
  }

  list_tabledata_callback(project, dataset, table, append_rows,
    table_info = table_info, page_size = page_size, max_pages = max_pages,
    warn = warn, quiet = quiet
  )

  do.call("rbind", rows)
}

#' @rdname list_tabledata
#' @export
list_tabledata_callback <- function(project, dataset, table, callback,
                                    table_info = NULL,
                                    page_size = 1e4, max_pages = 10,
                                    warn = TRUE,
                                    quiet = getOption("bigquery.quiet")) {
  assert_that(is.string(project), is.string(dataset), is.string(table))
  assert_that(is.function(callback))
  assert_that(is.numeric(max_pages), length(max_pages) == 1, max_pages >= 1)

  elapsed <- timer()
  is_quiet <- function(x) isTRUE(quiet) || (is.na(quiet) && elapsed() < 2)

  if (!is_quiet()) cat("Retrieving data")
  table_info <- table_info %||% get_table(project, dataset, table)
  schema <- table_info$schema

  url <- sprintf("projects/%s/datasets/%s/tables/%s/data", project, dataset,
    table)
  cur_page <- 1
  rows_fetched <- 0

  req <- bq_get(url, query = list(maxResults = page_size))
  data <- extract_data(req$rows, schema)
  callback(data)
  if (!is.null(data)) {
    rows_fetched <- rows_fetched + nrow(data)
  }
  is_complete <- function(rows_fetched) rows_fetched >= as.integer(req$totalRows)

  while(cur_page < max_pages && !is_complete(rows_fetched)) {
    if (!is_quiet()) {
      cat("\rRetrieving data: ", sprintf("%4.1f", elapsed()), "s", sep = "")
    }

    req <- bq_get(url, query = list(
      pageToken = req$pageToken,
      maxResults = page_size)
    )
    data <- extract_data(req$rows, schema)
    callback(data)
    if (!is.null(data)) {
      rows_fetched <- rows_fetched + nrow(data)
    }

    cur_page <- cur_page + 1
  }
  if (!is_quiet()) cat("\n")

  if (isTRUE(warn) && !is_complete(rows_fetched)) {
    warning("Only first ", max_pages, " pages of size ", page_size,
      " retrieved. Use max_pages = Inf to retrieve all.", call. = FALSE)
  }

  invisible(TRUE)
}

#Types can be loaded into R, record is not supported yet.
converter <- list(
  integer = as.integer,
  float = as.double,
  boolean = as.logical,
  string = identity,
  timestamp = function(x) as.POSIXct(as.integer(x), origin = "1970-01-01", tz = "UTC")
)

extract_data <- function(rows, schema) {
  if (is.null(rows)) return(NULL)

  types <- tolower(vapply(schema$fields, function(x) x$type, character(1)))

  # Convert NULLs into NAs
  out <- character(length(rows) * length(types))
  for(i in seq_along(rows)) {
    for(j in seq_along(types)) {
      if (is.null(rows[[i]]$f[[j]]$v)) rows[[i]]$f[[j]]$v <- NA_character_
    }
  }
  data <- unlist(rows, use.names = FALSE)
  data_m <- matrix(data, nrow = length(types))

  out <- vector("list", length(types))
  for(i in seq_along(types)) {
    type <- types[[i]]
    if (!(type %in% names(converter))) {
      stop("Don't know how to convert type ", type, call. = FALSE)
    }
    out[[i]] <- converter[[type]](data_m[i, ])
  }

  names(out) <- vapply(schema$fields, function(x) x$name, character(1))
  as_df(out)
}
