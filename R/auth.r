#' @importFrom httr oauth_endpoint oauth_app oauth2.0_token
google <- oauth_endpoint(NULL, "auth", "token",
  base_url = "https://accounts.google.com/o/oauth2")
bigqr <- oauth_app("google",
  "465736758727.apps.googleusercontent.com",
  "fJbIIyoIag0oA6p114lwsV2r")

bq_env <- new.env(parent = emptyenv())


#' Get and set access credentials
#'
#' Since the majority of bigquery API requests need to be authenticated
#' bigrquery maintains package-wide OAuth authentication credentials in a
#' private environment. In ordinary operation, you should never need to use
#' these functions but they are provided in case you want to switch
#' credentials mid-stream.
#'
#' @section API console:
#' To manage your google projects, use the API console:
#' \url{https://console.cloud.google.com/}
#'
#' @keywords internal
#' @export
#' @param value new access credentials, as returned by
#'  \code{\link[httr]{oauth2.0_token}}
get_access_cred <- function() {
  cred <- bq_env$access_cred
  if (is.null(cred)) {
    cred <- oauth2.0_token(google, bigqr,
      scope = c(
          "https://www.googleapis.com/auth/bigquery",
          "https://www.googleapis.com/auth/cloud-platform"))

    # Stop if unsuccessful
    set_access_cred(cred)
  }

  cred
}

#' @rdname get_access_cred
#' @export
set_access_cred <- function(value) {
  bq_env$access_cred <- value
}

#' @rdname get_access_cred
#' @export
reset_access_cred <- function() {
  set_access_cred(NULL)
}

get_sig <- function() {
  stop("Deprecated: use get_access_cred directly", call. = FALSE)
}

# The service_token can be an absolute or relative filename, or JSON text
set_service_token <- function(service_token) {

  service_token <- jsonlite::fromJSON(service_token)

  endpoint <- httr::oauth_endpoints("google")

  scope <- "https://www.googleapis.com/auth/bigquery"

  cred <- httr::oauth_service_token(endpoint, service_token, scope)

  set_access_cred(cred)
}

