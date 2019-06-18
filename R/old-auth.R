#' Deprecated functions for access credentials
#'
#' bigrquery's main functions for managing credentials are now [bq_auth()] and
#' [bq_auth_configure()]. The functions documented here are now deprecated and
#' will eventually be removed.
#'
#' @keywords internal
#' @name bigrquery-deprecated
NULL

#' @rdname bigrquery-deprecated
#' @export
#' @return `get_access_cred()` returns the current OAuth2 credentials token.
get_access_cred <- function() {
  .Deprecated("bq_auth() or bq_token()", package = "bigrquery")
  if (!bq_has_token()) {
    bq_auth()
  }
  .auth$get_cred()
}

#' @rdname bigrquery-deprecated
#' @param value New access credentials, as returned by [httr::oauth2.0_token()].
#' @export
set_access_cred <- function(value) {
  .Deprecated("bq_auth(token = ...)", package = "bigrquery")
  .auth$set_cred(value)
}

#' @rdname bigrquery-deprecated
#' @export
reset_access_cred <- function() {
  .Deprecated("bq_deauth()", package = "bigrquery")
  .auth$clear_cred()
}

#' @rdname bigrquery-deprecated
#' @keywords internal
#' @export
has_access_cred <- function() {
  .Deprecated("bq_has_token()", package = "bigrquery")
  bq_has_token()
}

#' @rdname bigrquery-deprecated
#' @param app A Google OAuth application created with [httr::oauth_app()].
#' @export
set_oauth2.0_cred <- function(app) {
  .Deprecated(msg = glue("
     Use `bq_auth_configure()` to configure your own OAuth app. That will
     dictate the app used when `bq_auth()` is called implicitly or explicitly to
     obtain an OAuth2 token.
  "))
  cred <- gargle::gargle2.0_token(
    scopes = c(
      "https://www.googleapis.com/auth/bigquery",
      "https://www.googleapis.com/auth/cloud-platform"
    ),
    app = app,
    package = "bigrquery"
  )
  bq_auth(token = cred)
}

#' @rdname bigrquery-deprecated
#' @param service_token A JSON string, URL or file, providing a service account
#'   token.
#' @export
set_service_token <- function(service_token) {
  .Deprecated("bq_auth(path = ...)", package = "bigrquery")
  bq_auth(path = service_token)
}
