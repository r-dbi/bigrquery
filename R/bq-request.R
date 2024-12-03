base_url <- "https://bigquery.googleapis.com/bigquery/v2/"
upload_url <- "https://bigquery.googleapis.com/upload/bigquery/v2/"

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
  check_string(project, allow_null = TRUE)
  check_string(dataset, allow_null = TRUE)
  check_string(table, allow_null = TRUE)

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
    "httr2/", utils::packageVersion("httr2")
  )
}

bq_body <- function(body, ...) {
  user <- toCamel(list(...))
  utils::modifyList(body, user)
}

#' @importFrom httr2 req_perform resp_body_json resp_body_raw
bq_get <- function(url, ..., query = NULL, raw = FALSE, token = bq_token()) {
  req <- bq_request(url, "GET", token, query = query)
  resp <- req_perform(req)
  if (!raw) {
    resp_body_json(resp)
  } else {
    resp_body_raw(resp)
  }
}

bq_exists <- function(url, query = NULL, token = bq_token()) {
  req <- bq_request(url, "GET", token, query = query)
  resp <- req_perform(req)
  # A 404 is not an error here.
  req <- req_error(req, is_error = function(resp) {
    resp_status(resp) != 404 && resp_status(resp) >= 400
  })
  resp <- req_perform(req)
  resp_status(resp) != 404
}

bq_get_paginated <- function(url, query = NULL, token = bq_token(),
                             page_size = 50, max_pages = Inf, warn = TRUE) {

  check_number_whole(max_pages, min = 1, allow_infinite = TRUE)
  check_number_whole(page_size, min = 1)

  if (!is.null(query$fields))
    query$fields <- paste0(query$fields, ",nextPageToken")

  query <- utils::modifyList(list(maxResults = page_size), query %||% list())
  pages <- list()

  page <- bq_get(url, query = query, token = token)
  i <- 1
  pages[[i]] <- page
  page_token <- page$nextPageToken

  while (!is.null(page_token) && i < max_pages) {
    query$pageToken <- page_token
    page <- bq_get(url, query = query, token = token)

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

#' @importFrom httr2 req_perform
bq_delete <- function(url, query = NULL, token = bq_token()) {
  req <- bq_request(url, "DELETE", token, query = query)
  req_perform(req)
  invisible(NULL)
}

#' @importFrom httr2 req_body_json req_perform resp_body_json
bq_post <- function(url, body, query = NULL, token = bq_token()) {
  req <- bq_request(url, "POST", token, query = query)
  req <- req_body_json(req, body)
  resp <- req_perform(req)
  invisible(resp_body_json(resp))
}

#' @importFrom httr2 req_body_json req_perform resp_body_json
bq_patch <- function(url, body, query = NULL, token = bq_token()) {
  req <- bq_request(url, "PATCH", token, query = query)
  req <- req_body_json(req, body)
  resp <- req_perform(req)
  resp_body_json(resp)
}

#' @importFrom httr2 req_body_json req_body_raw req_perform resp_body_json
# https://cloud.google.com/bigquery/docs/reference/api-uploads
bq_upload <- function(url, metadata, media, query = list(), token = bq_token()) {

  query <-  utils::modifyList(list(fields = "jobReference",uploadType = "resumable"), query)

  req <- bq_request(url, "POST", token, query = query, base_url = upload_url)
  req <- req_body_json(req, metadata[["content"]])
  resp <- req_perform(req)

  # Note: We only get here if the request above is successful.
  session_uri <- httr2::resp_header(resp, "Location")
  req <- bq_request(session_uri, "PUT", token, query = query)
  req <- req_body_raw(req, media[["content"]], type = media[["type"]])
  resp <- req_perform(req)
  resp_body_json(resp)
}

#' @importFrom httr2 request req_user_agent req_url_path_append req_method req_auth_bearer_token req_url_query req_error req_perform
bq_request <- function(url,
                       method,
                       token,
                       query = NULL,
                       base_url = base_url,
                       call = caller_env()) {
  req <- request(base_url)
  req <- req_user_agent(req, bq_ua())
  req <- req_url_path_append(req, url)
  req <- req_method(req, method)
  req <- req_auth_bearer_token(req, token)
  if (!is.null(query)) {
    req <- req_url_query(req, !!!prepare_bq_query(query))
  }
  req_error(req, body = bq_error_body)
}

#' @importFrom httr2 resp_content_type resp_status resp_body_json resp_body_string
bq_error_body <- function(resp) {
  # Generic error message for non-JSON responses.
  if (resp_content_type(resp) != "application/json") {
    message <- paste0(
      "HTTP error [",
      resp_status(resp),
      "]\n",
      resp_body_string(resp)
    )
    return(message)
  }

  body <- resp_body_json(resp)
  reason <- body$error$errors[[1L]]$reason
  message <- body$error$message
  if (!is.null(reason)) {
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
  }
  message
}
