#' Download table data
#'
#' This retrieves rows in chunks of `page_size`. It is most suitable for results
#' of smaller queries (<100 MB, say). For larger queries, it is better to
#' export the results to a CSV file stored on google cloud and use the
#' bq command line tool to download locally.
#'
#' @section Complex data:
#' bigrquery will retrieve nested and repeated columns in to list-columns
#' as follows:
#'
#' * Repeated values (arrays) will become a list-cols of vectors.
#' * Records will become list-cols of named lists.
#' * Repeated records will become list-cols of data frames.
#'
#' @section Larger datasets:
#' In my timings, this code takes around 1 minute per 100 MB of data.
#' If you need to download considerably more than this, I recommend:
#'
#'  * Export a `.csv` file to Cloud Storage using [bq_table_save()]
#'  * Use the `gsutil` command line utility to download it
#'  * Read the csv file into R with `readr::read_csv()` or `data.table::fread()`.
#'
#'  Unfortunately you can not export nested or repeated formats into CSV, and
#'  the formats that BigQuery supports (arvn and ndjson) that allow for
#'  nested/repeated values, are not well supported in R.
#'
#' @return Because data retrieval may generalise list-cols and the data frame
#'   print method can have problems with list-cols, this method returns
#'   tibbles. If you need a data frame, coerce the results with
#'   `as.data.frame()`.
#' @param x A [bq_table]
#' @param max_results Maximum number of results to retrieve. Use `Inf`
#'   retrieve all rows.
#' @param page_size The number of rows returned per page. Make this smaller
#'   if you have many fields or large records and you are seeing a
#'   'responseTooLarge' error.
#' @param start_index Starting row index (zero-based).
#' @param max_connections Number of maximum simultaneously connections to
#'   BigQuery servers.
#' @inheritParams api-job
#' @param bigint The R type that BigQuery's 64-bit integer types should be mapped to.
#'   The default is `"integer"` which returns R's `integer` type but results in `NA` for
#'   values above/below +/- 2147483647. `"integer64"` returns a [bit64::integer64],
#'   which allows the full range of 64 bit integers.
#' @section Google BigQuery API documentation:
#' * [list](https://cloud.google.com/bigquery/docs/reference/rest/v2/tabledata/list)
#' @export
#' @examples
#' if (bq_testable()) {
#'   df <- bq_table_download("publicdata.samples.natality", max_results = 35000)
#' }
bq_table_download <-
  function(x,
           max_results = Inf,
           page_size = 1e4,
           start_index = 0L,
           max_connections = 6L,
           quiet = NA,
           bigint = c("integer", "integer64", "numeric", "character")) {
    x <- as_bq_table(x)
    assert_that(is.numeric(page_size), length(page_size) == 1)
    assert_that(is.numeric(max_results), length(max_results) == 1)
    assert_that(is.numeric(start_index), length(start_index) == 1)
    bigint <- match.arg(bigint)

    nrow <- bq_table_nrow(x)

    schema_path <- bq_download_schema(x, tempfile())
    page_paths <- bq_download_pages(x,
                                    max_results = max_results,
                                    start_index = start_index,
                                    quiet = quiet
    )
    on.exit(file.remove(c(schema_path, page_paths)))

    table_data <- bq_parse_files(schema_path,
                                 page_paths,
                                 n = nrow,
                                 quiet = bq_quiet(quiet)
    )
    convert_bigint(table_data, bigint)
  }

# This function is a modified version of
# https://github.com/r-dbi/RPostgres/blob/master/R/PqResult.R
convert_bigint <- function(df, bigint) {
  if (bigint == "integer64") {
    return(df)
  }

  as_bigint <- switch(bigint,
                      integer = as.integer,
                      numeric = as.numeric,
                      character = as.character
  )

  rapply_int64(df, f = as_bigint)
}
rapply_int64 <- function(x, f) {
  if (is.list(x)) {
    x[] <- lapply(x, rapply_int64, f = f)
    x
  } else if (bit64::is.integer64(x)) {
    f(x)
  } else {
    x
  }
}

bq_download_pages <- function(x,
                              max_results = Inf,
                              start_index = 0,
                              quiet = NA) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, dataset = x$dataset, table = x$table, data = "")
  # Pre-format query params with forced non-scientific notation, since the BQ
  # API doesn't accept numbers like 1e5. See issue #395 for details.
  query <- list(
    startIndex = format(start_index, scientific = FALSE),
    maxResults = format(max_results, scientific = FALSE)
  )

  progress <- bq_progress("Downloading data [:spinner]", quiet = quiet)

  page <- NULL
  i <- 1

  out_dir <- tempfile("bq-download")
  dir.create(out_dir)

  repeat({
    query$pageToken <- page
    resp <- bq_get(url, query = query)

    # Can we get pageToken without having to parse all data slowly?
    jsonlite::write_json(resp, file.path(out_dir, paste("bq-", i, ".json")))

    if (is.null(resp$pageToken)) {
      break
    }

    page <- resp$pageToken
    i <- i + 1
    progress$tick()
  })

  dir(out_dir, full.names = TRUE)
}

# Helpers for testing -----------------------------------------------------

bq_parse_file <- function(fields, data) {
  fields <- readr::read_file(fields)
  data <- readr::read_file(data)

  bq_parse(fields, data)
}

bq_download_schema <- function(x, path) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, x$table)
  query <- list(fields = "schema")

  json <- bq_get(url, query = query, raw = TRUE)
  writeBin(json, path)

  path
}
