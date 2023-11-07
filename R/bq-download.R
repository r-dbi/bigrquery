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
#' * Repeated values (arrays) will become a list-column of vectors.
#' * Records will become list-columns of named lists.
#' * Repeated records will become list-columns of data frames.
#'
#' @section Larger datasets:
#' In my timings, this code takes around 1 minute per 100 MB of data.
#' If you need to download considerably more than this, I recommend:
#'
#'  * Export a `.csv` file to Cloud Storage using [bq_table_save()].
#'  * Use the `gsutil` command line utility to download it.
#'  * Read the csv file into R with `readr::read_csv()` or `data.table::fread()`.
#'
#'  Unfortunately you can not export nested or repeated formats into CSV, and
#'  the formats that BigQuery supports (arvn and ndjson) that allow for
#'  nested/repeated values, are not well supported in R.
#'
#' @return Because data retrieval may generate list-columns and the `data.frame`
#'   print method can have problems with list-columns, this method returns
#'   a tibble. If you need a `data.frame`, coerce the results with
#'   [as.data.frame()].
#' @param x A [bq_table]
#' @param n_max Maximum number of results to retrieve. Use `Inf` to retrieve all
#'   rows.
#' @param page_size The number of rows requested per chunk. It is recommended to
#'   leave this unspecified until you have evidence that the `page_size`
#'   selected automatically by `bq_table_download()` is problematic.
#'
#'   When `page_size = NULL` bigrquery determines a conservative, natural chunk
#'   size empirically. If you specify the `page_size`, it is important that each
#'   chunk fits on one page, i.e. that the requested row limit is low enough to
#'   prevent the API from paginating based on response size.
#' @param start_index Starting row index (zero-based).
#' @param max_connections Number of maximum simultaneous connections to
#'   BigQuery servers.
#' @inheritParams api-job
#' @param bigint The R type that BigQuery's 64-bit integer types should be
#'   mapped to. The default is `"integer"`, which returns R's `integer` type,
#'   but results in `NA` for values above/below +/- 2147483647. `"integer64"`
#'   returns a [bit64::integer64], which allows the full range of 64 bit
#'   integers.
#' @param max_results `r lifecycle::badge("deprecated")` Deprecated. Please use
#'   `n_max` instead.
#' @section Google BigQuery API documentation:
#' * [list](https://cloud.google.com/bigquery/docs/reference/rest/v2/tabledata/list)
#' @export
#' @examples
#' if (bq_testable()) {
#'   df <- bq_table_download("publicdata.samples.natality", n_max = 35000)
#' }
bq_table_download <-
  function(x,
           n_max = Inf,
           page_size = NULL,
           start_index = 0L,
           max_connections = 6L,
           quiet = NA,
           bigint = c("integer", "integer64", "numeric", "character"),
           max_results = deprecated()) {
    x <- as_bq_table(x)
    bigint <- match.arg(bigint)
    if (lifecycle::is_present(max_results)) {
      lifecycle::deprecate_warn(
        "1.4.0", "bq_table_download(max_results)", "bq_table_download(n_max)"
      )
      n_max <- max_results
    }

    params <- set_row_params(
      nrow = bq_table_nrow(x),
      n_max = n_max,
      start_index = start_index
    )
    n_max <- params$n_max
    start_index <- params$start_index

    schema_path <- bq_download_schema(x, tempfile())
    withr::defer(file.remove(schema_path))

    if (n_max == 0) {
      table_data <- bq_parse_files(
        schema_path,
        file_paths = character(),
        n = 0,
        quiet = bq_quiet(quiet)
      )
      return(table_data)
    }

    pool <- curl::new_pool()

    # get first chunk ----
    if (!bq_quiet(quiet)) {
      message("Downloading first chunk of data.")
    }

    if (is.null(page_size)) {
      chunk_size_from_user <- FALSE
    } else {
      assert_that(
        is.numeric(page_size),
        length(page_size) == 1,
        page_size > 0
      )
      chunk_size_from_user <- TRUE
    }
    chunk_size <- page_size

    chunk_plan <- bq_download_plan(
      n_max,
      chunk_size = chunk_size,
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
      done = bq_download_callback(chunk_plan$dat$path[1]),
      pool = pool
    )
    curl::multi_run(pool = pool)
    path_first_chunk <- chunk_plan$dat$path[1]
    withr::defer(file.remove(path_first_chunk))

    chunk_data <- bq_parse_file(schema_path, path_first_chunk)
    n_got <- nrow(chunk_data)

    if (n_got >= n_max) {
      if (!bq_quiet(quiet)) {
        message("First chunk includes all requested rows.")
      }
      return(convert_bigint(chunk_data, bigint))
    }

    if (chunk_size_from_user && n_got < chunk_size) {
      abort(c(
        "First chunk is incomplete:",
        x = glue("{big_mark(chunk_size)} rows were requested, but only \\
                  {big_mark(n_got)} rows were received."),
        i = "Leave `page_size` unspecified or use an even smaller value."
      ))
    }

    # break rest of work into natural chunks ----
    if (!chunk_size_from_user) {
      if (!bq_quiet(quiet)) {
        message(glue("Received {big_mark(n_got)} rows in the first chunk."))
      }
      chunk_size <- trunc(0.75 * n_got)
    }

    n_max_new <- n_max - n_got
    start_index_new <- n_got

    chunk_plan <- bq_download_plan(
      n_max_new,
      chunk_size = chunk_size,
      start_index = start_index_new
    )
    progress <- bq_progress(
      "Downloading data [:bar] :percent ETA: :eta",
      total = chunk_plan$n_chunks,
      quiet = quiet
    )

    if (!bq_quiet(quiet)) {
      message(glue_data(
        chunk_plan,
        "Downloading the remaining {big_mark(n_max)} rows in {n_chunks} \\
         chunks of (up to) {big_mark(chunk_size)} rows."
      ))
    }

    for (i in seq_len(chunk_plan$n_chunks)) {
      handle <- bq_download_chunk_handle(
        x,
        begin = chunk_plan$dat$chunk_begin[i],
        max_results = chunk_plan$dat$chunk_rows[i]
      )
      curl::multi_add(
        handle,
        done = bq_download_callback(chunk_plan$dat$path[i], progress),
        pool = pool
      )
    }
    curl::multi_run(pool = pool)
    withr::defer(file.remove(chunk_plan$dat$path))

    table_data <- bq_parse_files(
      schema_path,
      c(path_first_chunk, chunk_plan$dat$path),
      n = n_max,
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

set_row_params <- function(nrow, n_max = Inf, start_index = 0L) {
  assert_that(is.numeric(n_max), length(n_max) == 1, n_max >= 0)
  assert_that(is.numeric(start_index), length(start_index) == 1, start_index >= 0)

  n_max <- max(min(n_max, nrow - start_index), 0)

  list(n_max = n_max, start_index = start_index)
}

bq_download_plan <- function(n_max,
                             chunk_size = NULL,
                             n_chunks = NULL,
                             start_index = 0) {
  params <- set_chunk_params(n_max, chunk_size, n_chunks)
  list(
    n_max      = n_max,
    chunk_size = params$chunk_size,
    n_chunks   = params$n_chunks,
    dat = set_chunk_plan(
      n_max,
      params$chunk_size,
      params$n_chunks,
      start_index
    )
  )
}

set_chunk_params <- function(n_max, chunk_size = NULL, n_chunks = NULL) {
  if (is.null(chunk_size) && is.null(n_chunks)) {
    n_chunks <- 1
  }
  n_chunks <- n_chunks %||% Inf
  chunk_size <- pmin(chunk_size %||% ceiling(n_max / n_chunks), n_max)
  n_chunks <- pmin(n_chunks, ceiling(n_max / chunk_size))
  list(chunk_size = chunk_size, n_chunks = n_chunks)
}

set_chunk_plan <- function(n_max, chunk_size, n_chunks, start_index = 0) {
  chunk_begin <- start_index + (seq_len(n_chunks) - 1) * chunk_size
  chunk_end <- pmin(chunk_begin + chunk_size, start_index + n_max)
  chunk_rows <- chunk_end - chunk_begin
  tibble::tibble(
    chunk_begin,
    chunk_rows,
    path = sort(
      tempfile(rep_len("bq-download-", length.out = n_chunks), fileext = ".json")
    )
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

bq_download_callback <- function(path, progress = NULL) {
  force(path)
  function(result) {
    if (!is.null(progress)) progress$tick()

    bq_check_response(
      result$status_code,
      curl::parse_headers_list(result$headers)[["content-type"]],
      result$content
    )

    con <- file(path, open = "wb")
    withr::defer(close(con))
    writeBin(result$content, con)
  }
}

# Helpers for testing -----------------------------------------------------

bq_parse_file <- function(fields, data) {
  fields <- brio::read_file(fields)
  data <- brio::read_file(data)

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
