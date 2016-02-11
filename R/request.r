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

#' @importFrom httr GET config
bq_get <- function(url, ..., query = NULL, token = get_access_cred()) {
  req <- GET(paste0(base_url, url), config(token = token), ...,
             query = prepare_bq_query(query))
  process_request(req)
}

#' @importFrom httr DELETE config
bq_delete <- function(url, ..., query = NULL, token = get_access_cred()) {
  req <- DELETE(paste0(base_url, url), config(token = token), ...,
                query = prepare_bq_query(query))
  process_request(req)
}

#' @importFrom httr POST add_headers config
bq_post <- function(url, body, ..., query = NULL, token = get_access_cred()) {
  json <- jsonlite::toJSON(body)
  req <- POST(paste0(base_url, url), body = json, config(token = token),
              add_headers("Content-Type" = "application/json"), ...,
              query = prepare_bq_query(query))
  process_request(req)
}

#' @importFrom httr PUT add_headers config
bq_put <- function(url, body, ..., query = NULL, token = get_access_cred()) {
  json <- jsonlite::toJSON(body)
  req <- PUT(paste0(base_url, url), body = json, config(token = token),
              add_headers("Content-Type" = "application/json"), ...,
              query = prepare_bq_query(query))
  process_request(req)
}

#' @importFrom httr POST add_headers config
bq_upload <- function(url, parts, ..., query = NULL, token = get_access_cred()) {
  url <- paste0(upload_url, url)
  req <- POST_multipart_related(url, parts = parts, config(token = token), ...,
                                query = prepare_bq_query(query))
  process_request(req)
}


#' @importFrom httr http_status content parse_media status_code
process_request <- function(req) {
  # No content -> success
  if (status_code(req) == 204) return(TRUE)

  if (status_code(req) >= 200 && status_code(req) < 300) {
    return(content(req, "parsed", "application/json"))
  }

  type <- parse_media(req$headers$`Content-type`)
  if (type$complete == "application/json") {
    out <- content(req, "parsed", "application/json")
    signal_reason(out$error$errors[[1L]]$reason, out$error$message)
  } else {
    out <- content(req, "text")
    stop("HTTP error [", req$status, "] ", out, call. = FALSE)
  }
}

signal_reason <- function(reason, message) {
  if (!is.null(reason)) {
    cl <- c(paste0("bigrquery_", reason), "error", "condition")
    cond <- structure(list(message = message), class = cl)
    signalCondition(cond)
  }

  stop(message, call. = FALSE)
}

# Multipart/related ------------------------------------------------------------


# http://www.w3.org/Protocols/rfc1341/7_2_Multipart.html
POST_multipart_related <- function(url, config = NULL, parts = NULL, ...,
                                   boundary = random_boundary(),
                                   handle = NULL) {
  if (is.null(config)) config <- config()

  sep <- paste0("\n--", boundary, "\n")
  end <- paste0("\n--", boundary, "--\n")

  body <- paste0(sep, paste0(parts, collapse = sep), end)

  type <- paste0("multipart/related; boundary=", boundary)
  config <- c(config, add_headers("Content-Type" = type))

  POST(url, config = config, body = body,
    query = list(uploadType = "multipart"), ..., handle = handle)
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

