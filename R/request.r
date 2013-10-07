base_url <- "https://www.googleapis.com/bigquery/v2/"
upload_url <- "https://www.googleapis.com/upload/bigquery/v2/"

#' @importFrom httr GET config
bq_get <- function(url, config = NULL, ..., sig = get_sig()) {
  if (is.null(config)) {
    config <- config()
  }
  config <- c(config, sig)
  req <- GET(paste0(base_url, url), config, ...)
  process_request(req)
}

#' @importFrom httr POST add_headers config
#' @importFrom RJSONIO toJSON
bq_post <- function(url, body, config = NULL, ..., sig = get_sig()) {
  if (is.null(config)) {
    config <- config()
  }
  json <- toJSON(body)
  config <- c(config, sig, add_headers("Content-type" = "application/json"))

  req <- POST(paste0(base_url, url), config, body = json, ...)
  process_request(req)
}

#' @importFrom httr POST add_headers config
#' @importFrom RJSONIO toJSON
bq_upload <- function(url, parts, config = NULL, ..., sig = get_sig()) {
  if (is.null(config)) {
    config <- config()
  }
  
  config <- c(config, sig)
  
  url <- paste0(upload_url, url)
  req <- POST_multipart_related(url, config, parts = parts, ...)
  process_request(req)
}


#' @importFrom httr http_status content parse_media
process_request <- function(req) {
  if (http_status(req)$category == "success") {
    return(content(req, "parsed", "application/json"))
  }

  type <- parse_media(req$headers$`Content-type`)
  if (type$complete == "application/json") {
    out <- content(req, "parsed", "application/json")
    stop(out$err$message, call. = FALSE)
  } else {
    out <- content(req, "text")
    stop("HTTP error [", req$status, "] ", out, call. = FALSE)
  }
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
  
  POST(url, config = config, body = body, query = list(uploadType = "multipart"), ..., handle = handle)
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

