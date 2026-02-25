## This file is the interface between bigrquery and the
## auth functionality in gargle.

# Initialization happens in .onLoad
.auth <- NULL

## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE = "bigrquery",
  YOUR_STUFF = "your BigQuery projects",
  PRODUCT = "Google BigQuery",
  API = "BigQuery API",
  PREFIX = "bq"
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
#'
#' @importFrom gargle token_fetch
bq_auth <- function(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = c(
    "https://www.googleapis.com/auth/bigquery",
    "https://www.googleapis.com/auth/cloud-platform"
  ),
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
) {
  if (!missing(email) && !missing(path)) {
    cli::cli_warn(c(
      "It is very unusual to provide both {.arg email} and \\
       {.arg path} to {.fun bq_auth}.",
      "They relate to two different auth methods.",
      "The {.arg path} argument is only for a service account token.",
      "If you need to specify your own OAuth client, use \\
      {.fun bq_auth_configure}."
    ))
  }

  # In a BYO token situation, such as `bq_auth(token = drive_token())`, it's
  # easy to not have, e.g., googledrive attached (provides drive_token()).
  # If we don't force here, the error is muffled in token_fetch()'s tryCatch()
  # treatment, which makes it much harder to figure out what's wrong.
  # By forcing here, we expose this mistake early and noisily.
  force(token)

  cred <- token_fetch(
    scopes = scopes,
    client = bq_oauth_client() %||% gargle::tidyverse_client(),
    email = email,
    path = path,
    package = "bigrquery",
    cache = cache,
    use_oob = use_oob,
    token = token
  )
  if (!inherits(cred, "Token2.0")) {
    cli::cli_abort(c(
      "Can't get Google credentials.",
      i = if (!is_interactive()) {
        "Try calling {.fun bq_auth} directly with necessary specifics."
      }
    ))
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

has_internal_auth <- function() {
  gargle::secret_has_key("BIGRQUERY_KEY")
}

bq_auth_internal <- function() {
  path <- system.file("secret", "bigrquery-testing.json", package = "bigrquery")
  json <- gargle::secret_decrypt_json(path, "BIGRQUERY_KEY")
  bq_auth(path = json)
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
#' @param client A Google OAuth client, presumably constructed via
#'   [gargle::gargle_oauth_client_from_json()]. Note, however, that it is
#'   preferred to specify the client with JSON, using the `path` argument.
#' @inheritParams gargle::gargle_oauth_client_from_json
#' @eval gargle:::PREFIX_auth_configure_return(gargle_lookup_table, .has_api_key = FALSE)
#'
#' @family auth functions
#' @export
#' @examples
#' # see and store the current user-configured OAuth client (probably `NULL`)
#' (original_client <- bq_oauth_client())
#'
#' # the preferred way to configure your own client is via a JSON file
#' # downloaded from Google Developers Console
#' # this example JSON is indicative, but fake
#' path_to_json <- system.file(
#'   "extdata", "data", "client_secret_123.googleusercontent.com.json",
#'   package = "bigrquery"
#' )
#' bq_auth_configure(path = path_to_json)
#'
#' # confirm the changes
#' bq_oauth_client()
#'
#' # restore original auth config
#' bq_auth_configure(client = original_client)
bq_auth_configure <- function(client, path) {
  check_exclusive(client, path)
  if (!missing(path)) {
    check_string(path)
    client <- gargle::gargle_oauth_client_from_json(path)
  } else {
    if (!is.null(client) && !inherits(client, "gargle_oauth_client")) {
      stop_input_type(client, "a gargle OAuth client", allow_null = TRUE)
    }
  }

  .auth$set_client(client)
  invisible(.auth)
}

#' @export
#' @rdname bq_auth_configure
bq_oauth_client <- function() {
  .auth$client
}

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
