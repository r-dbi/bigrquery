#' @importFrom httr GET
bq_get <- function(url, config = list(), ..., sig = get_sig()) {
  config <- c(config, sig)
  req <- GET(paste0(base_url, url), config, ...)
  process_request(req)
}

#' @importFrom httr POST add_headers
#' @importFrom RJSONIO toJSON
bq_post <- function(url, body, config = list(), ..., sig = get_sig()) {
  json <- toJSON(body)
  config <- c(config, sig, add_headers("Content-type" = "application/json"))

  req <- POST(paste0(base_url, url), config, body = json, ...)
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
