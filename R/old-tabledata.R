#' Retrieve data from a table [deprecated]
#'
#' Please use [bq_table_download] instead.
#'
#' @param callback function called with single argument, the data from the
#'   current page of data
#' @param quiet if `FALSE`, prints informative status messages.
#' @param table_info if known, the table information retrieved with
#'   [get_table()]
#' @param page_size Number of items per page.
#' @param warn If `TRUE`, warn when there are rows remaining to
#'   be pulled down from database.
#' @param max_pages maximum number of pages to retrieve. Use `Inf`
#'  to retrieve the complete dataset.
#' @seealso API documentation at
#'   \url{https://cloud.google.com/bigquery/docs/reference/rest/v2/tabledata/list}
#' @return `list_tabledata` returns a single dataframe.
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' billing_project <- "341409650721" # put your project number here
#' natal <- list_tabledata("publicdata", "samples", "natality", max_pages = 2,
#'   page_size = 10)
#' dim(natal)
#' }
list_tabledata <- function(project, dataset, table,
                           page_size = 1e4,
                           table_info = NULL,
                           max_pages = 10,
                           warn = TRUE,
                           quiet = getOption("bigrquery.quiet")
                           ) {

  .Deprecated("bq_table_download", package = "bigrquery")

  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Must install readr package to use `list_tabledata", call. = FALSE)
  }

  assert_that(is.string(project), is.string(dataset), is.string(table))
  assert_that(is.numeric(max_pages), length(max_pages) == 1)

  if (max_pages < 1) {
    table_info <- table_info %||% get_table(project, dataset, table)
    rows <- extract_data(NULL, table_info$schema)
    return(rows)
  }

  # This is a rather inefficient implementation - better strategy would be
  # preallocate list when max_pages is finite, and use doubling strategy
  # when it's not.
  rows <- list()
  append_rows <- function(new_rows) {
    rows <<- c(rows, list(new_rows))
  }

  list_tabledata_callback(
    project,
    dataset,
    table,
    append_rows,
    table_info = table_info,
    page_size = page_size,
    max_pages = max_pages,
    warn = warn,
    quiet = quiet
  )

  do.call("rbind", rows)
}

#' @description
#' `list_tabledata_callback` calls the supplied callback with each page
#' of data.
#' @rdname list_tabledata
#' @export
list_tabledata_callback <- function(project, dataset, table, callback,
                                    table_info = NULL,
                                    page_size = getOption("bigrquery.page.size"),
                                    max_pages = 10,
                                    warn = TRUE,
                                    quiet = getOption("bigrquery.quiet")) {
  assert_that(is.string(project), is.string(dataset), is.string(table))
  assert_that(is.function(callback))
  assert_that(is.numeric(max_pages), length(max_pages) == 1, max_pages >= 1)

  iter <- list_tabledata_iter(
    project = project,
    dataset = dataset,
    table = table,
    table_info = table_info
  )

  pages <- min(ceiling(iter$get_rows() / page_size), max_pages)
  progress <- bq_progress(
    "Retrieving data [:bar] :percent eta: :eta",
    total = max(pages, 1),
    quiet = quiet
  )

  cur_page <- 0L

  while (cur_page < max_pages && !iter$is_complete()) {
    data <- iter$next_(page_size)
    callback(data)

    progress$tick()
    cur_page <- cur_page + 1
  }

  if (isTRUE(warn) && !iter$is_complete()) {
    warning("Only first ", max_pages, " pages of size ", page_size,
            " retrieved. Use max_pages = Inf to retrieve all.", call. = FALSE)
  }

  invisible(TRUE)
}

#' @rdname list_tabledata
#' @export
list_tabledata_iter <- function(project, dataset, table, table_info = NULL) {

  table_info <- table_info %||% get_table(project, dataset, table)
  schema <- table_info$schema

  url <- bq_path(project, dataset = dataset, table = table, data = "")

  last_response <- NULL
  rows_fetched <- 0

  next_ <- function(n) {
    query <- list(maxResults = n)
    query$pageToken <- last_response$pageToken

    response <- bq_get(url, query = query)

    data <- extract_data(response$rows, schema)
    rows_fetched <<- rows_fetched + nrow(data)

    # Record only page token and total number of rows to reduce memory consumption
    last_response <<- response[c("pageToken", "totalRows")]

    data
  }

  is_complete <- function() {
    !is.null(last_response) && rows_fetched >= as.numeric(last_response$totalRows)
  }

  next_paged <- function(n, page_size = getOption("bigrquery.page.size")) {
    if (is_complete()) {
      return(extract_data(NULL, schema))
    }

    target_rows_fetched <- rows_fetched + n

    ret <- list()
    repeat {
      next_n <- min(page_size, target_rows_fetched - rows_fetched)
      chunk <- next_(next_n)

      # This has O(n^2) aggregated run time, but fetching large data from
      # BigQuery will be slow for other reasons
      ret <- c(ret, list(chunk))

      if (is_complete() || rows_fetched >= target_rows_fetched) {
        break
      }
    }
    do.call(rbind, ret)
  }

  get_schema <- function() {
    schema
  }

  get_rows_fetched <- function() {
    rows_fetched
  }

  get_rows <- function() {
    as.numeric(table_info$numRows)
  }

  #' @return
  #' `list_tabledata_iter` returns a named list with functions
  #' * `next_` (fetches one chunk of rows)
  #' * `next_paged` (fetches arbitrarily many rows using a specified page size)
  #' * `is_complete` (checks if all rows have been fetched)
  #' * `get_schema` (returns the schema of the table),
  #' * `get_rows_fetched` (returns the number of rows already fetched).
  #' * `get_rows` (returns total number of rows)
  list(
    next_ = next_,
    next_paged = next_paged,
    is_complete = is_complete,
    get_schema = get_schema,
    get_rows = get_rows,
    get_rows_fetched = get_rows_fetched
  )
}

#Types can be loaded into R, record is not supported yet.
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


extract_data <- function(rows, schema) {
  if (is.null(rows) || length(rows) == 0L) {
    # Corner case: Zero rows
    dummy_rows <- list(list(f = rep(list(NULL), length(schema$fields))))

    data <- extract_data(dummy_rows, schema)
    return(data[0L, , drop = FALSE])
  }

  types <- tolower(vapply(schema$fields, function(x) x$type, character(1)))

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

  names(out) <- vapply(schema$fields, function(x) x$name, character(1))
  as_df(out)
}


null_to_na <- function(x) {
  .Call(null_to_na_, x)
}


