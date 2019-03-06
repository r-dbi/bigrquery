# nocov start

#' Deprecated functions for access credentials
#'
#' bigrquery's main functions for managing credentials are now [bq_auth()] and
#' [bq_auth_config()]. The functions documented here are now deprecated and will
#' eventually be removed.
#'
#' @keywords internal
#' @name deprecated-auth
NULL

#' @rdname deprecated-auth
#' @export
#' @return `get_access_cred()` returns the current OAuth2 credentials token.
get_access_cred <- function() {
  .Deprecated("bq_auth")
  if (!has_access_cred()) {
    bq_auth()
  }
  .auth$get_cred()
}

#' @rdname deprecated-auth
#' @param value New access credentials, as returned by [httr::oauth2.0_token()].
#' @export
set_access_cred <- function(value) {
  .Deprecated("GOOD_QUESTION")
  .auth$set_cred(value)
}

#' @rdname deprecated-auth
#' @export
reset_access_cred <- function() {
  .Deprecated("GOOD_QUESTION")
  .auth$clear_cred()
}

#' @rdname deprecated-auth
#' @param app A Google OAuth application created with [httr::oauth_app()].
#' @export
set_oauth2.0_cred <- function(app = NULL) {
  ## jennybc: I'm not entirely sure of this function's role. I am assuming it
  ## exists as a way to "Bring Your Own OAuth App".
  .Deprecated("bq_auth_config")
  cred <- httr::oauth2.0_token(
    endpoint = httr::oauth_endpoints("google"),
    app %||% bq_app,
    scope = c(
      "https://www.googleapis.com/auth/bigquery",
      "https://www.googleapis.com/auth/cloud-platform"
    )
  )
  .auth$set_cred(cred)
}

#' @rdname deprecated-auth
#' @param service_token A JSON string, URL or file, providing a service account
#'   token.
#' @export
set_service_token <- function(service_token) {
  .Deprecated("bq_auth")
  bq_auth(path = service_token)
}

# nocov end
