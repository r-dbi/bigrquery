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
#' credentials mid-stream. You may can use `set_service_token`
#' for non-interactive authentication.
#'
#' @section API console:
#' To manage your google projects, use the API console:
#' \url{https://console.cloud.google.com/}
#'
#' @keywords internal
#' @export
#' @param value new access credentials, as returned by
#'  [httr::oauth2.0_token()]
get_access_cred <- function() {
  cred <- bq_env$access_cred
  if (is.null(cred)) {
    set_oauth2.0_cred()
  }

  bq_env$access_cred
}

#' @rdname get_access_cred
#' @param app A Google OAuth application created using
#'  \code{\link[httr]{oauth_app}}
#' @export
set_oauth2.0_cred <- function(app = NULL) {
  if (is.null(app)) {
    app <- bigqr
  }

  cred <- oauth2.0_token(google, app,
    scope = c(
        "https://www.googleapis.com/auth/bigquery",
        "https://www.googleapis.com/auth/cloud-platform"))

  set_access_cred(cred)
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


#' @export
#' @rdname get_access_cred
#' @param service_token A JSON string, URL or file, giving or pointing to
#'   the service token file.
set_service_token <- function(service_token) {

  service_token <- jsonlite::fromJSON(service_token)

  endpoint <- httr::oauth_endpoints("google")

  scope <- "https://www.googleapis.com/auth/bigquery"

  cred <- httr::oauth_service_token(endpoint, service_token, scope)

  set_access_cred(cred)
}

