## This file is the interface between bigrquery and the
## auth functionality in gargle.
.auth <- gargle::init_AuthState(
  package     = "bigrquery",
  auth_active = TRUE
)

bq_app <- function() bqoa()

## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "bigrquery",
  YOUR_STUFF  = "your BigQuery projects",
  PRODUCT     = "Google BigQuery",
  API         = "BigQuery API",
  PREFIX      = "bq",
  AUTH_CONFIG_SOURCE = "bigrquery"
)

#' Authorize bigrquery
#'
#' @eval gargle:::PREFIX_auth_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_details(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params()
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
                    cache = gargle::gargle_oauth_cache(),
                    use_oob = gargle::gargle_oob_default(),
                    token = NULL) {
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = bq_oauth_app() %||% bq_app(),
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
#' Reports whether bigrquery has stored a token, ready for use in downstream
#' requests.
#'
#' @return Logical.
#' @export
#'
#' @examples
#' bq_has_token()
bq_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}

#' @rdname deprecated-auth
#' @keywords internal
#' @export
has_access_cred <- function() {
  .Deprecated("bq_has_token")
  bq_has_token()
}

#' View or edit auth config
#'
#' @eval gargle:::PREFIX_auth_config_description(
#'   gargle_lookup_table, .deauth_possible = FALSE
#' )
#' @eval gargle:::PREFIX_auth_config_params_except_key(gargle_lookup_table)
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
    stop("Don't provide both 'app' and 'path'. Pick one.", call. = FALSE)
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
