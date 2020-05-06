base_url <- "https://www.googleapis.com/bigquery/v2/"
upload_url <- "https://www.googleapis.com/upload/bigquery/v2/"

prepare_bq_query <- function(query) {
  api_key <- Sys.getenv("BIGRQUERY_API_KEY")
  if (!nzchar(api_key)) {
    return(query)
  }
  query <- query %||% list()
  query[["key"]] <- query[["key"]] %||% api_key
  query
}

bq_path <- function(project, dataset = NULL, table = NULL, ...) {
  assert_that(is.null(project) || is.string(project))
  assert_that(is.null(table) || is.string(table))
  assert_that(is.null(dataset) || is.string(dataset))

  components <- c(
    projects = project,
    datasets = dataset,
    tables = table,
    ...
  )

  paste0(names(components), "/", components, collapse = "/")
}

bq_ua <- function() {
  paste0(
    "bigrquery/", utils::packageVersion("bigrquery"), " ",
    "(GPN:RStudio; )", " ",
    "gargle/", utils::packageVersion("gargle"), " ",
    "httr/", utils::packageVersion("httr")
  )
}

bq_body <- function(body, ...) {
  user <- toCamel(list(...))
  utils::modifyList(body, user)
}


#' @importFrom httr GET config
bq_get <- function(url, ..., query = NULL, raw = FALSE, token = bq_token()) {
  req <- GET(
    paste0(base_url, url),
    token,
    httr::user_agent(bq_ua()),
    ...,
    query = prepare_bq_query(query)
  )
  process_request(req, raw = raw)
}

bq_exists <- function(url, ..., query = NULL, token = bq_token()) {
  req <- GET(
    paste0(base_url, url),
    token,
    httr::user_agent(bq_ua()),
    ...,
    query = prepare_bq_query(query)
  )
  status_code(req) >= 200 && status_code(req) < 300
}


#' @importFrom httr GET config
bq_get_paginated <- function(url, ..., query = NULL, token = bq_token(),
                             page_size = 50, max_pages = Inf, warn = TRUE) {

  assert_that(is.numeric(max_pages), length(max_pages) == 1)
  assert_that(is.numeric(page_size), length(page_size) == 1)

  if (!is.null(query$fields))
    query$fields <- paste0(query$fields, ",nextPageToken")

  query <- utils::modifyList(list(maxResults = page_size), query %||% list())
  pages <- list()

  page <- bq_get(url, ..., query = query, token = token)
  i <- 1
  pages[[i]] <- page
  page_token <- page$nextPageToken

  while (!is.null(page_token) && i < max_pages) {
    query$pageToken <- page_token
    page <- bq_get(url, ..., query = query, token = token)

    i <- i + 1
    pages[[i]] <- page
    page_token <- page$nextPageToken
  }

  if (isTRUE(warn) && !is.null(page_token)) {
    warning(
      "Only first ", big_mark(max_pages * page_size), " results retrieved. ",
      "Adjust with `max_pages` and `page_size` parameters.",
      call. = FALSE
    )
  }

  pages
}


#' @importFrom httr DELETE config
bq_delete <- function(url, ..., query = NULL, token = bq_token()) {
  req <- DELETE(
    paste0(base_url, url),
    token,
    httr::user_agent(bq_ua()),
    ...,
    query = prepare_bq_query(query)
  )
  process_request(req)
}

#' @importFrom httr POST add_headers config
bq_post <- function(url, body, ..., query = NULL, token = bq_token()) {
  json <- jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE)

  req <- POST(
    paste0(base_url, url),
    body = json,
    httr::user_agent(bq_ua()),
    token,
    add_headers("Content-Type" = "application/json"),
    ...,
    query = prepare_bq_query(query)
  )
  invisible(process_request(req))
}

#' @importFrom httr PATCH add_headers config
bq_patch <- function(url, body, ..., query = NULL, token = bq_token()) {
  json <- jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE)
  req <- PATCH(
    paste0(base_url, url),
    body = json,
    httr::user_agent(bq_ua()),
    token,
    add_headers("Content-Type" = "application/json"),
    ...,
    query = prepare_bq_query(query)
  )
  process_request(req)
}

#' @importFrom httr POST add_headers config
bq_upload <- function(url, parts, ..., query = list(), token = bq_token()) {
  url <- paste0(upload_url, url)
  req <- POST_multipart_related(
    url,
    parts = parts,
    token,
    httr::user_agent(bq_ua()),
    ...,
    query = prepare_bq_query(query)
  )
  process_request(req)
}


#' @importFrom httr http_status content parse_media status_code
process_request <- function(req, raw = FALSE) {
  status <- status_code(req)
  # No content -> success
  if (status == 204) return(TRUE)

  type <- req$headers$`Content-type`
  content <- content(req, "raw")

  bq_check_response(status, type, content)

  if (raw) {
    content
  } else {
    jsonlite::fromJSON(rawToChar(content), simplifyVector = FALSE)
  }
}

bq_check_response <- function(status, type, content) {
  if (status >= 200 && status < 300) {
    return()
  }

  type <- httr::parse_media(type)
  if (type$complete == "application/json") {
    json <- jsonlite::fromJSON(rawToChar(content), simplifyVector = FALSE)
    signal_reason(json$error$errors[[1L]]$reason, json$error$message)
  } else {
    text <- rawToChar(content)
    stop("HTTP error [", status, "] ", text, call. = FALSE)
  }
}

signal_reason <- function(reason, message) {
  if (is.null(reason)) {
    rlang::abort(message)
  } else {
    advice <- NULL
    if (reason == "responseTooLarge") {
      # If message mentions "allowLargeResults", that's the right advice to
      # follow. But other times we get the error when a single page of results
      # is too large (e.g. for tables with many columns). By decreasing page
      # size, a single page of results is now small enough to return.
      if (!any(grepl("allowLargeResults", message, fixed = TRUE))) {
        advice <- "Try decreasing the `page_size` value of `bq_table_download()`"
      }
    } else if (reason == "rateLimitExceeded") {
      # bigrquery pulls results in parallel to increase throughput.
      # "rateLimitExceeded" can happen when those threads are making too many
      # requests within a brief time interval. Can slow threads down by asking
      # for bigger pages.
      advice <- "Try increasing the `page_size` value of `bq_table_download()`"
    }
    message <- c(
      paste0(message, " [", reason, "] "),
      i = advice
    )

    rlang::abort(message, class = paste0("bigrquery_", reason))
  }
}

# Multipart/related ------------------------------------------------------------


# http://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
POST_multipart_related <- function(url, config = NULL, parts = NULL,
                                   query = list(), ...,
                                   boundary = random_boundary(),
                                   handle = NULL) {
  if (is.null(config)) config <- config()

  sep <- paste0("\n--", boundary, "\n")
  end <- paste0("\n--", boundary, "--\n")

  body <- paste0(sep, paste0(parts, collapse = sep), end)

  type <- paste0("multipart/related; boundary=", boundary)
  config <- c(config, add_headers("Content-Type" = type))

  query <- utils::modifyList(list(uploadType = "multipart"), query)

  POST(url, config = config, body = body, query = query, ..., handle = handle)
}

part <- function(headers, body) {
  if (length(headers) == 0) {
    header <- "\n"
  } else {
    header <- paste0(names(headers), ": ", headers, "\n", collapse = "")
  }
  body <- paste0(body, collapse = "\n")

  paste0(header, "\n", body)
}

random_boundary <- function() {
  valid <- c(LETTERS, letters, 0:9) # , "'", "(", ")", "+", ",", "-", ".", "/",
  #  ":", "?")
  paste0(sample(valid, 50, replace = TRUE), collapse = "")
}

