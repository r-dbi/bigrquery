#' Download table data
#'
#' @param x A [bq_table]
#' @param max_results Maximum nubmer of results to return
#' @param page_size The number of rows returned per page. Make this smaller
#'   if you have many fields or large records and you are seeing a
#'   "responseTooLarge" error.
#' @param max_pages Maximum number of pages to retrieve. Use `Inf` retrieve all
#'   rows.
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

  n_pages <- floor(max_results / page_size) + 1L
  pages <- vector("list", n_pages)

  start <- start_index
  for (i in 1:n_pages) {
    pages[[i]] <- bq_table_download_page(x,
      start_index = start,
      max_results = min(page_size, max_results - start),
      fields = fields
    )

    start <- start + page_size
  }

  do.call("rbind", pages)
}

bq_table_download_page <- function(x,
                                   start_index = 0L,
                                   max_results = 1e4,
                                   fields = NULL
                                   ) {

  x <- as_bq_table(x)
  assert_that(is.numeric(max_results), length(max_results) == 1)
  assert_that(is.numeric(start_index), length(start_index) == 1)
  fields <- fields %||% bq_table_fields(x)

  url <- bq_path(x$project, dataset = x$dataset, table = x$table, data = "")
  query <- list(
    startIndex = start_index,
    maxResults = max_results
  )

  resp <- bq_get(url, query = query)
  bq_extract_data(resp$rows, fields)
}

bq_extract_data <- function(rows, fields) {
  if (is.null(rows) || length(rows) == 0L) {
    # Corner case: Zero rows
    dummy_rows <- list(list(f = rep(list(NULL), length(fields))))

    data <- bq_extract_data(dummy_rows, fields)
    return(data[0L, , drop = FALSE])
  }

  types <- tolower(vapply(fields, function(x) x$type, character(1)))

  # Convert NULLs into NAs
  rows <- null_to_na(rows)
  data <- unlist(rows, use.names = FALSE)
  data_m <- matrix(data, nrow = length(types))

  out <- vector("list", length(types))
  converter <- converters()
  for (i in seq_along(types)) {
    type <- types[[i]]
    if (!(type %in% names(converter))) {
      stop("Don't know how to convert type ", type, call. = FALSE)
    }
    out[[i]] <- converter[[type]](data_m[i, ])
  }

  names(out) <- map_chr(fields, function(x) x$name)
  as_df(out)
}


null_to_na <- function(x) {
  .Call(null_to_na_, x)
}

converters <- function() {
  list(
    integer = as.integer,
    float = as.double,
    boolean = as.logical,
    string = identity,
    timestamp = function(x) {
      as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "UTC")
    },
    time = function(x) {
      readr::parse_time(x, "%H:%M:%OS")
    },
    date = function(x) {
      readr::parse_date(x, format = "%Y-%m-%d")
    },
    datetime = function(x) {
      readr::parse_datetime(x)
    }
  )
}
