#' Download table data
#'
#' This retrieves rows in chunks of `page_size`. It is most suitable for results
#' of smaller queries (<100 meg, say). For larger queries, it is better to
#' export the results to a CSV file stored on google cloud and use the
#' bq command line tool to download locally.
#'
#' @param x A [bq_table]
#' @param max_results Maximum number of results to retrieve. Use `Inf`
#'   retrieve all rows.
#' @param page_size The number of rows returned per page. Make this smaller
#'   if you have many fields or large records and you are seeing a
#'   "responseTooLarge" error.
#' @param start_index Starting row index (zero-based).
#' @section API documentation:
#' * [list](https://developers.google.com/bigquery/docs/reference/v2/tabledata/list)
#' @export
#' @examples
#' if (bq_testable()) {
#' df <- bq_table_download("publicdata.samples.natality", max_results = 35000)
#' }
bq_table_download <- function(x, max_results = Inf, page_size = 1e4, start_index = 0L) {
  x <- as_bq_table(x)
  assert_that(is.numeric(page_size), length(page_size) == 1)
  assert_that(is.numeric(max_results), length(max_results) == 1)
  assert_that(is.numeric(start_index), length(start_index) == 1)

  if (!is.finite(max_results)) {
    max_results <- bq_table_size(x) - start_index
  }
  fields <- bq_table_fields(x)

  n_pages <- ceiling(max_results / page_size)
  if (n_pages == 0) {
    return(bq_tabledata_to_data_frame(NULL, fields))
  }

  pages <- vector("list", n_pages)

  start <- start_index
  for (i in seq_len(n_pages)) {
    pages[[i]] <- bq_table_download_page(x,
      fields = fields,
      start_index = start,
      max_results = min(page_size, max_results - (start - start_index))
    )

    start <- start + page_size
  }

  do.call("rbind", pages)
}

bq_table_download_page <- function(x,
                                   fields,
                                   start_index = 0L,
                                   max_results = 1e4
                                   ) {

  x <- as_bq_table(x)
  assert_that(is.numeric(max_results), length(max_results) == 1)
  assert_that(is.numeric(start_index), length(start_index) == 1)

  url <- bq_path(x$project, dataset = x$dataset, table = x$table, data = "")
  query <- list(
    startIndex = start_index,
    maxResults = max_results
  )

  json <- bq_get(url, query = query, raw = TRUE)
  out <- bq_tabledata_to_list(json)

  bq_tabledata_to_data_frame(out, fields)
}

bq_tabledata_to_data_frame <- function(out, fields) {
  if (length(out) == 0) {
    out <- rep(list(character()), length(fields))
  }

  types <- map_chr(fields, function(x) x$type)
  out <- Map(bq_parse_type, types, out)

  class(out) <- "data.frame"
  names(out) <- map_chr(fields, function(x) x$name)
  attr(out, "row.names") <- c(NA_integer_, -length(out[[1]]))

  out
}

bq_parse_type <- function(type, x) {
  switch(type,
    INTEGER = as.integer(x),
    FLOAT = as.double(x),
    BOOLEAN = as.logical(x),
    TIMESTAMP = as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC"),
    TIME = readr::parse_time(x, "%H:%M:%OS"),
    DATE = readr::parse_date(x, format = "%Y-%m-%d"),
    DATETIME = readr::parse_datetime(x),
    x
  )
}
