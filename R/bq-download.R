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
           page_size = NULL,
           start_index = 0L,
           max_connections = 6L,
           quiet = NA,
           bigint = c("integer", "integer64", "numeric", "character")) {
    x <- as_bq_table(x)
    assert_that(is.numeric(max_results), length(max_results) == 1)
    if (!is.null(page_size)) assert_that(is.numeric(page_size), length(page_size) == 1)
    assert_that(is.numeric(start_index), length(start_index) == 1)
    bigint <- match.arg(bigint)

    schema_path <- bq_download_schema(x, tempfile())
    withr::defer(file.remove(schema_path))

    user_n_max <- max_results
    user_chunk_size <- page_size
    nrow <- bq_table_nrow(x)
    n_max <- pmax(pmin(user_n_max, nrow - start_index), 0)

    if (n_max == 0) {
      table_data <- bq_parse_files(
        schema_path,
        file_paths = character(),
        n = 0,
        quiet = bq_quiet(quiet)
      )
      return(table_data)
    }

    # general download prep ----
    pool <- curl::new_pool()
    bq_download_callback <- function(i, progress = NULL) {
      force(i)
      function(result) {
        if (!is.null(progress)) progress$tick()

        bq_check_response(
          result$status_code,
          curl::parse_headers_list(result$headers)[["content-type"]],
          result$content
        )

        token <- bq_peek_next_page(result)
        chunk_plan$dat$next_page[i] <<- token

        con <- file(chunk_plan$dat$path[i], open = "wb")
        withr::defer(close(con))
        writeBin(result$content, con)
      }
    }

    # get first chunk ----
    if (!bq_quiet(quiet)) {
      message("Downloading first chunk of data.")
    }

    chunk_plan <- bq_download_plan(
      n_max,
      chunk_size = user_chunk_size,
      n_chunks = 1,
      start_index = start_index
    )
    handle <- bq_download_chunk_handle(
      x,
      begin = chunk_plan$dat$chunk_begin[1],
      max_results = chunk_plan$dat$chunk_rows[1]
    )
    curl::multi_add(
      handle,
      done = bq_download_callback(1),
      pool = pool
    )
    curl::multi_run(pool = pool)
    chunk_data <- bq_parse_file(schema_path, chunk_plan$dat$path[1])
    n_got <- nrow(chunk_data)

    if (!nzchar(chunk_plan$dat$next_page[1]) || n_got >= n_max) {
      message("First chunk is all we need.")
      return(convert_bigint(chunk_data, bigint))
    }

    # break rest of work into natural chunks ----
    chunk_size <- trunc(0.75 * n_got)
    message(glue("First chunk has {n_got} rows."))

    chunk_plan <- bq_download_plan(
      n_max,
      chunk_size = chunk_size,
      # TODO: start where we left off?
      start_index = start_index
    )
    progress <- bq_progress(
      "Downloading data [:bar] :percent ETA: :eta",
      total = chunk_plan$n_chunks,
      quiet = quiet
    )

    if (!bq_quiet(quiet)) {
      message(glue_data(
        chunk_plan,
        "Downloading {big_mark(n_max)} rows in {n_chunks} chunks \\
         of (at most) {chunk_size[1]} rows."
      ))
    }

    for (i in seq_len(chunk_plan$n_chunks)) {
      handle <-bq_download_chunk_handle(
        x,
        begin = chunk_plan$dat$chunk_begin[i],
        max_results = chunk_plan$dat$chunk_rows[i]
      )
      curl::multi_add(
        handle,
        done = bq_download_callback(i, progress),
        pool = pool
      )
    }
    curl::multi_run(pool = pool)
    withr::defer(file.remove(chunk_plan$dat$path))

    table_data <- bq_parse_files(
      schema_path,
      chunk_plan$dat$path,
      n = chunk_plan$n_max,
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

bq_download_plan <- function(n_max,
                             chunk_size = NULL,
                             n_chunks = NULL,
                             start_index = 0) {
  if (is.null(chunk_size) && is.null(n_chunks)) {
    rlang::abort("`chunk_size` and/or `n_chunks` must be specified")
  }
  if (is.null(n_chunks)) {
    n_chunks <- n_chunks %||% Inf
  } else {
    assert_that(is.numeric(n_chunks), length(n_chunks) == 1)
    chunk_size <- chunk_size %||% ceiling(n_max / n_chunks)
  }
  n_chunks <- pmin(n_chunks, ceiling(n_max / chunk_size))

  chunk_begin <- start_index + (seq_len(n_chunks) - 1) * chunk_size
  chunk_end <- pmin(chunk_begin + chunk_size, n_max)
  chunk_rows <- chunk_end - chunk_begin
  dat <- tibble::tibble(
    chunk_begin,
    chunk_rows,
    path = sort(
      tempfile(rep_len("bq-download-", length.out = n_chunks), fileext = ".json")
    ),
    next_page = rep_len("", length.out = n_chunks)
  )

  list(
    n_max = n_max,
    chunk_size = chunk_size,
    n_chunks = n_chunks,
    dat = dat
  )
}

bq_download_chunk_handle <- function(x, begin = 0L, max_results = 1e4) {
  x <- as_bq_table(x)
  assert_that(is.numeric(begin), length(begin) == 1)
  assert_that(is.numeric(max_results), length(max_results) == 1)

  # Pre-format query params with forced non-scientific notation, since the BQ
  # API doesn't accept numbers like 1e5. See issue #395 for details.
  query <- list(
    startIndex = format(begin, scientific = FALSE),
    maxResults = format(max_results, scientific = FALSE)
  )

  url <- paste0(base_url, bq_path(x$project, dataset = x$dataset, table = x$table, data = ""))
  url <- httr::modify_url(url, query = prepare_bq_query(query))
  # cat("\nurl", url, "\n")

  if (bq_has_token()) {
    token <- .auth$get_cred()
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

bq_peek_next_page <- function(result) {
  fragment <- rawToChar(readBin(result$content, n = 300, what = "raw"))
  tmp <- strsplit(fragment, split = "\n")[[1]]
  tmp <- grep("pageToken", tmp, value = TRUE)
  if (length(tmp) == 0) {
    return("")
  }
  tmp <- strsplit(tmp, split = ":")[[1]][[2]]
  sub('.*["](.*)["].*', "\\1", tmp)
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
