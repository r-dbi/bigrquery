# https://code.google.com/apis/console

base_url <- "https://www.googleapis.com/bigquery/v2/"

#' @importFrom httr oauth_endpoint oauth_app oauth2.0_token
google <- oauth_endpoint(NULL, "auth", "token",
  base_url = "https://accounts.google.com/o/oauth2")
bigqr <- oauth_app("google",
  "465736758727.apps.googleusercontent.com",
  "fJbIIyoIag0oA6p114lwsV2r")

if (!exists("bq_env")) {
  bq_env <- new.env(parent = emptyenv())
}

set_access_cred <- function(value) bq_env$access_cred <- value
get_access_cred <- function() {
  cred <- bq_env$access_cred
  if (is.null(cred)) {
    cred <- oauth2.0_token(google, bigqr,
      scope = "https://www.googleapis.com/auth/bigquery")

    # Stop if unsuccesful
    set_access_cred(cred)
  }

  cred
}

#' @importFrom httr sign_oauth2.0
get_sig <- function() {
  sign_oauth2.0(get_access_cred()$access_token)
}
