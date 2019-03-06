## This file is the interface between bigrquery and the
## auth functionality in gargle.

bq_app <- httr::oauth_app(
  "google",
  "465736758727.apps.googleusercontent.com",
  "fJbIIyoIag0oA6p114lwsV2r"
)

.auth <- gargle::AuthState$new(
  package     = "bigrquery",
  ## FIXME(jennybc): which app to use? tidyverse or existing bigrquery app?
  # app       = gargle::tidyverse_app(),
  app         = bq_app,
  api_key     = NULL,
  auth_active = TRUE,
  cred        = NULL
)

## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "bigrquery",
  YOUR_STUFF  = "your BigQuery projects",
  PRODUCT     = "Google BigQuery",
  API         = "BigQuery API",
  PREFIX      = "bq",
  SCOPES_LINK = "https://developers.google.com/identity/protocols/googlescopes#bigqueryv2"
)

#' Authorize bigrquery
#'
#' @eval gargle:::PREFIX_auth_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_details(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params_email()
#' @eval gargle:::PREFIX_auth_params_path()
#' @eval gargle:::PREFIX_auth_params_scopes(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params_cache_use_oob()
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
#' ## see user associated with current token
#' bq_user()
#'
#' ## force use of a token associated with a specific email
#' bq_auth(email = "jenny@example.com")
#' bq_user()
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
bq_auth <- function(email = NULL,
                    path = NULL,
                    scopes = c(
                      "https://www.googleapis.com/auth/bigquery",
                      "https://www.googleapis.com/auth/cloud-platform"
                    ),
                    cache = getOption("gargle.oauth_cache"),
                    use_oob = getOption("gargle.oob_default")) {
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = .auth$app,
    email = email,
    path = path,
    package = "bigrquery",
    cache = cache,
    use_oob = use_oob
  )
  if (!gargle::is_legit_token(cred, verbose = TRUE)) {
    stop(
      "Can't get Google credentials.\n",
      "Are you running bigrquery in a non-interactive session? Consider:\n",
      "  * Call `bq_auth()` directly with all necessary specifics.\n",
      call. = FALSE
    )
  }
  .auth$set_cred(cred)

  invisible()
}

#' @rdname deprecated-auth
#' @keywords internal
#' @export
has_access_cred <- function() {
  ## jennybc: I'd prefer to NOT export this, but it was previously exported
  ## I won't deprecate because it is called internally
  ## since it needs to be documented, I'm documenting with the deprecated auth
  ## functions
  .auth$has_cred()
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
#' req <- request_generate(
#'   "drive.files.get",
#'   list(fileId = "abc"),
#'   token = drive_token()
#' )
#' req
#' }
bq_token <- function() {
  if (!has_access_cred()) {
    bq_auth()
  }
  httr::config(token = .auth$get_cred())
}

#' View or edit auth config
#'
#' @eval gargle:::PREFIX_auth_config_description(
#'   gargle_lookup_table, .deauth_possible = FALSE
#' )
#' @eval gargle:::PREFIX_auth_config_params_except_key()
#' @eval gargle:::PREFIX_auth_config_return_without_key(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examples
#' ## this will print current config
#' bq_auth_config()
#'
#' if (require(httr)) {
#'   ## bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   bq_auth_config(app = google_app)
#' }
#'
#' \dontrun{
#' ## bring your own app via JSON downloaded from Google Developers Console
#' bq_auth_config(
#'   path = "/path/to/the/JSON/you/downloaded/from/google/dev/console.json"
#' )
#' }
bq_auth_config <- function(app = NULL,
                           path = NULL) {
  stopifnot(is.null(app) || inherits(app, "oauth_app"))
  stopifnot(is.null(path) || is_string(path))

  if (!is.null(app) && !is.null(path)) {
    stop_glue("Don't provide both 'app' and 'path'. Pick one.")
  }

  if (is.null(app) && !is.null(path)) {
    app <- gargle::oauth_app_from_json(path)
  }
  if (!is.null(app)) {
    .auth$set_app(app)
  }

  .auth
}

#' @export
#' @rdname bq_auth_config
bq_oauth_app <- function() .auth$app
