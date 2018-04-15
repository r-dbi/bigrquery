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
bq_table_download <- function(x,
                              max_results = Inf,
                              page_size = 1e4,
                              start_index = 0L,
                              max_connections = 6L,
                              quiet = NA) {
  x <- as_bq_table(x)
  assert_that(is.numeric(page_size), length(page_size) == 1)
  assert_that(is.numeric(max_results), length(max_results) == 1)
  assert_that(is.numeric(start_index), length(start_index) == 1)

  if (!is.finite(max_results)) {
    max_results <- bq_table_nrow(x) - start_index
  }

  schema_path <- tempfile()
  bq_table_save_schema(x, schema_path)

  n_pages <- ceiling(max_results / page_size)
  if (n_pages == 0) {
    stop("Implement me")
  }

  paths <- tempfile(rep("bq-", n_pages), fileext = ".json")
  cons <- lapply(paths, file, open = "wb")
  on.exit(lapply(cons, close), add = TRUE)

  pool <- curl::new_pool(host_con = max_connections)
  start <- start_index

  progress <- bq_progress(
    "Downloading data [:bar] :percent eta: :eta",
    total = n_pages,
    quiet = quiet
  )

  for (i in seq_len(n_pages)) {
    max <- min(page_size, max_results - (start - start_index))
    start <- start + page_size

    h <- bq_table_page_handle(x, start_index = start, max_results = max)
    curl::multi_add(h, data = cons[[i]], function(h) progress$tick(), pool = pool)
  }

  curl::multi_run(pool = pool)

  bq_parse_files(schema_path, paths, n = max_results)
}

bq_table_page_handle <- function(x, start_index = 0L, max_results = 1e4) {
  x <- as_bq_table(x)
  assert_that(is.numeric(max_results), length(max_results) == 1)
  assert_that(is.numeric(start_index), length(start_index) == 1)

  query <- list(
    startIndex = start_index,
    maxResults = max_results
  )

  url <- paste0(base_url, bq_path(x$project, dataset = x$dataset, table = x$table, data = ""))
  url <- httr::modify_url(url, query = prepare_bq_query(query))

  token <- get_access_cred()
  if (!is.null(token)) {
    signed <- token$sign("GET", url)
    url <- signed$url
    headers <- signed$headers
  } else {
    headers <- list()
  }

  h <- curl::new_handle(url = url)
  curl::handle_setopt(h, useragent = bq_ua())
  curl::handle_setheaders(h, .list = headers)

  h
}

# Helpers for testing -----------------------------------------------------

bq_parse_file <- function(fields, data) {
  fields <- readr::read_file(fields)
  data <- readr::read_file(data)

  bq_parse(fields, data)
}

bq_table_save_values <- function(x, path, max_results = 100) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, dataset = x$dataset, table = x$table, data = "")
  query <- list(
    startIndex = 0,
    maxResults = max_results
  )

  json <- bq_get(url, query = query, raw = TRUE)
  writeBin(json, path)
}

bq_table_save_schema <- function(x, path) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, x$table)
  query <- list(fields = "schema")

  json <- bq_get(url, query = query, raw = TRUE)
  writeBin(json, path)
}
