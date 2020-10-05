## This file is the interface between bigrquery and the
## auth functionality in gargle.
.auth <- gargle::init_AuthState(
  package     = "bigrquery",
  auth_active = TRUE
)

## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "bigrquery",
  YOUR_STUFF  = "your BigQuery projects",
  PRODUCT     = "Google BigQuery",
  API         = "BigQuery API",
  PREFIX      = "bq"
)

#' Authorize bigrquery
#'
#' @eval gargle:::PREFIX_auth_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_details(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params()
#' @param scopes A character vector of scopes to request.
#'   Pick from those listed at <https://developers.google.com/identity/protocols/oauth2/scopes>.
#'
#' @family auth functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## load/refresh existing credentials, if available
#' ## otherwise, go to browser for authentication and authorization
#' bq_auth()
#'
#' ## force use of a token associated with a specific email
#' bq_auth(email = "jenny@example.com")
#'
#' ## force a menu where you can choose from existing tokens or
#' ## choose to get a new one
#' bq_auth(email = NA)
#'
#' ## use a 'read only' scope, so it's impossible to change data
#' bq_auth(
#'   scopes = "https://www.googleapis.com/auth/devstorage.read_only"
#' )
#'
#' ## use a service account token
#' bq_auth(path = "foofy-83ee9e7c9c48.json")
#' }
bq_auth <- function(email = gargle::gargle_oauth_email(),
                    path = NULL,
                    scopes = c(
                      "https://www.googleapis.com/auth/bigquery",
                      "https://www.googleapis.com/auth/cloud-platform"
                    ),
                    cache = gargle::gargle_oauth_cache(),
                    use_oob = gargle::gargle_oob_default(),
                    token = NULL) {
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = bq_oauth_app() %||% gargle::tidyverse_app(),
    email = email,
    path = path,
    package = "bigrquery",
    cache = cache,
    use_oob = use_oob,
    token = token
  )
  if (!inherits(cred, "Token2.0")) {
    stop(
      "Can't get Google credentials.\n",
      "Are you running bigrquery in a non-interactive session? Consider:\n",
      "  * Call `bq_auth()` directly with all necessary specifics.\n",
      call. = FALSE
    )
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

#' Clear current token
#'
#' Clears any currently stored token. The next time bigrquery needs a token, the
#' token acquisition process starts over, with a fresh call to [bq_auth()] and,
#' therefore, internally, a call to [gargle::token_fetch()]. Unlike some other
#' packages that use gargle, bigrquery is not usable in a de-authorized state.
#' Therefore, calling `bq_deauth()` only clears the token, i.e. it does NOT
#' imply that subsequent requests are made with an API key in lieu of a token.
#'
#' @family auth functions
#' @export
#' @examples
#' \dontrun{
#' bq_deauth()
#' }
bq_deauth <- function() {
  .auth$clear_cred()
  invisible()
}

#' Produce configured token
#'
#' @eval gargle:::PREFIX_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_token_return()
#'
#' @family low-level API functions
#' @export
#' @examples
#' \dontrun{
#' bq_token()
#' }
bq_token <- function() {
  if (!bq_has_token()) {
    bq_auth()
  }
  httr::config(token = .auth$cred)
}

#' Is there a token on hand?
#'
#' @eval gargle:::PREFIX_has_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_has_token_return()
#'
#' @family low-level API functions
#' @export
#'
#' @examples
#' bq_has_token()
bq_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}

#' Edit and view auth configuration
#'
#' @eval gargle:::PREFIX_auth_configure_description(gargle_lookup_table, .has_api_key = FALSE)
#' @eval gargle:::PREFIX_auth_configure_params(.has_api_key = FALSE)
#' @eval gargle:::PREFIX_auth_configure_return(gargle_lookup_table, .has_api_key = FALSE)
#'
#' @family auth functions
#' @export
#' @examples
#' # see the current user-configured OAuth app (probaby `NULL`)
#' bq_oauth_app()
#'
#' if (require(httr)) {
#'
#'   # store current state, so we can restore
#'   original_app <- bq_oauth_app()
#'
#'   # bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   bq_auth_configure(app = google_app)
#'
#'   # confirm current app
#'   bq_oauth_app()
#'
#'   # restore original state
#'   bq_auth_configure(app = original_app)
#'   bq_oauth_app()
#' }
#'
#' \dontrun{
#' # bring your own app via JSON downloaded from GCP Console
#' bq_auth_configure(
#'   path = "/path/to/the/JSON/you/downloaded/from/gcp/console.json"
#' )
#' }
#'
bq_auth_configure <- function(app, path) {
  if (!xor(missing(app), missing(path))) {
    stop("Must supply exactly one of `app` and `path`", call. = FALSE)
  }
  if (!missing(path)) {
    stopifnot(is_string(path))
    app <- gargle::oauth_app_from_json(path)
  }
  stopifnot(is.null(app) || inherits(app, "oauth_app"))

  .auth$set_app(app)
}

#' @export
#' @rdname bq_auth_configure
bq_oauth_app <- function() .auth$app

#' Get info on current user
#'
#' @eval gargle:::PREFIX_user_description()
#' @eval gargle:::PREFIX_user_seealso()
#' @eval gargle:::PREFIX_user_return()
#'
#' @export
#' @examples
#' \dontrun{
#' bq_user()
#' }
bq_user <- function() {
  if (bq_has_token()) {
    gargle::token_email(bq_token())
  } else {
    NULL
  }
}
