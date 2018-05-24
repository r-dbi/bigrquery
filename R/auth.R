#' @importFrom httr oauth_endpoint oauth_app oauth2.0_token
bq_endpoint <- oauth_endpoint(NULL, "auth", "token",
  base_url = "https://accounts.google.com/o/oauth2"
)
bq_scopes <- c(
  "https://www.googleapis.com/auth/bigquery",
  "https://www.googleapis.com/auth/cloud-platform"
)
bq_app <- oauth_app(
  "google",
  "465736758727.apps.googleusercontent.com",
  "fJbIIyoIag0oA6p114lwsV2r"
)
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
#' @return `get_access_cred()` OAuth2 credentials token
get_access_cred <- function() {
  if (!has_access_cred()) {
    set_oauth2.0_cred()
  }
  bq_env$access_cred
}

#' @export
#' @return `has_access_cred()` TRUE if credentials are set
#' @rdname get_access_cred
has_access_cred <- function() {
  !is.null(bq_env$access_cred)
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


#' @rdname get_access_cred
#' @param app A Google OAuth application created using
#'  \code{\link[httr]{oauth_app}}
#' @export
# nocov start
set_oauth2.0_cred <- function(app = NULL) {
  cred <- oauth2.0_token(bq_endpoint, app %||% bq_app, scope = bq_scopes)
  set_access_cred(cred)
}
# nocov end

#' @export
#' @rdname get_access_cred
#' @param service_token A JSON string, URL or file, giving or pointing to
#'   the service token file.
set_service_token <- function(service_token) {
  service_token <- jsonlite::fromJSON(service_token)

  cred <- httr::oauth_service_token(bq_endpoint, service_token, scope = bq_scopes)
  set_access_cred(cred)
}
