# Authorize bigrquery

Authorize bigrquery to view and manage your BigQuery projects. This
function is a wrapper around
[`gargle::token_fetch()`](https://gargle.r-lib.org/reference/token_fetch.html).

By default, you are directed to a web browser, asked to sign in to your
Google account, and to grant bigrquery permission to operate on your
behalf with Google BigQuery. By default, with your permission, these
user credentials are cached in a folder below your home directory, from
where they can be automatically refreshed, as necessary. Storage at the
user level means the same token can be used across multiple projects and
tokens are less likely to be synced to the cloud by accident.

## Usage

``` r
bq_auth(
  email = gargle::gargle_oauth_email(),
  path = NULL,
  scopes = c("https://www.googleapis.com/auth/bigquery",
    "https://www.googleapis.com/auth/cloud-platform"),
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)
```

## Arguments

- email:

  Optional. If specified, `email` can take several different forms:

  - `"jane@gmail.com"`, i.e. an actual email address. This allows the
    user to target a specific Google identity. If specified, this is
    used for token lookup, i.e. to determine if a suitable token is
    already available in the cache. If no such token is found, `email`
    is used to pre-select the targeted Google identity in the OAuth
    chooser. (Note, however, that the email associated with a token when
    it's cached is always determined from the token itself, never from
    this argument).

  - `"*@example.com"`, i.e. a domain-only glob pattern. This can be
    helpful if you need code that "just works" for both
    `alice@example.com` and `bob@example.com`.

  - `TRUE` means that you are approving email auto-discovery. If exactly
    one matching token is found in the cache, it will be used.

  - `FALSE` or `NA` mean that you want to ignore the token cache and
    force a new OAuth dance in the browser.

  Defaults to the option named `"gargle_oauth_email"`, retrieved by
  [`gargle_oauth_email()`](https://gargle.r-lib.org/reference/gargle_options.html)
  (unless a wrapper package implements different default behavior).

- path:

  JSON identifying the service account, in one of the forms supported
  for the `txt` argument of
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  (typically, a file path or JSON string).

- scopes:

  A character vector of scopes to request. Pick from those listed at
  <https://developers.google.com/identity/protocols/oauth2/scopes>.

- cache:

  Specifies the OAuth token cache. Defaults to the option named
  `"gargle_oauth_cache"`, retrieved via
  [`gargle_oauth_cache()`](https://gargle.r-lib.org/reference/gargle_options.html).

- use_oob:

  Whether to use out-of-band authentication (or, perhaps, a variant
  implemented by gargle and known as "pseudo-OOB") when first acquiring
  the token. Defaults to the value returned by
  [`gargle_oob_default()`](https://gargle.r-lib.org/reference/gargle_options.html).
  Note that (pseudo-)OOB auth only affects the initial OAuth dance. If
  we retrieve (and possibly refresh) a cached token, `use_oob` has no
  effect.

  If the OAuth client is provided implicitly by a wrapper package, its
  type probably defaults to the value returned by
  [`gargle_oauth_client_type()`](https://gargle.r-lib.org/reference/gargle_options.html).
  You can take control of the client type by setting
  `options(gargle_oauth_client_type = "web")` or
  `options(gargle_oauth_client_type = "installed")`.

- token:

  A token with class
  [Token2.0](https://httr.r-lib.org/reference/Token-class.html) or an
  object of httr's class `request`, i.e. a token that has been prepared
  with [`httr::config()`](https://httr.r-lib.org/reference/config.html)
  and has a
  [Token2.0](https://httr.r-lib.org/reference/Token-class.html) in the
  `auth_token` component.

## Details

Most users, most of the time, do not need to call `bq_auth()` explicitly
– it is triggered by the first action that requires authorization. Even
when called, the default arguments often suffice.

However, when necessary, `bq_auth()` allows the user to explicitly:

- Declare which Google identity to use, via an `email` specification.

- Use a service account token or workload identity federation via
  `path`.

- Bring your own `token`.

- Customize `scopes`.

- Use a non-default `cache` folder or turn caching off.

- Explicitly request out-of-band (OOB) auth via `use_oob`.

If you are interacting with R within a browser (applies to RStudio
Server, Posit Workbench, Posit Cloud, and Google Colaboratory), you need
OOB auth or the pseudo-OOB variant. If this does not happen
automatically, you can request it explicitly with `use_oob = TRUE` or,
more persistently, by setting an option via
`options(gargle_oob_default = TRUE)`.

The choice between conventional OOB or pseudo-OOB auth is determined by
the type of OAuth client. If the client is of the "installed" type,
`use_oob = TRUE` results in conventional OOB auth. If the client is of
the "web" type, `use_oob = TRUE` results in pseudo-OOB auth. Packages
that provide a built-in OAuth client can usually detect which type of
client to use. But if you need to set this explicitly, use the
`"gargle_oauth_client_type"` option:

    options(gargle_oauth_client_type = "web")       # pseudo-OOB
    # or, alternatively
    options(gargle_oauth_client_type = "installed") # conventional OOB

For details on the many ways to find a token, see
[`gargle::token_fetch()`](https://gargle.r-lib.org/reference/token_fetch.html).
For deeper control over auth, use
[`bq_auth_configure()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md)
to bring your own OAuth client or API key. To learn more about gargle
options, see
[gargle::gargle_options](https://gargle.r-lib.org/reference/gargle_options.html).

## See also

Other auth functions:
[`bq_auth_configure()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md),
[`bq_deauth()`](https://bigrquery.r-dbi.org/dev/reference/bq_deauth.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## load/refresh existing credentials, if available
## otherwise, go to browser for authentication and authorization
bq_auth()

## force use of a token associated with a specific email
bq_auth(email = "jenny@example.com")

## force a menu where you can choose from existing tokens or
## choose to get a new one
bq_auth(email = NA)

## use a 'read only' scope, so it's impossible to change data
bq_auth(
  scopes = "https://www.googleapis.com/auth/devstorage.read_only"
)

## use a service account token
bq_auth(path = "foofy-83ee9e7c9c48.json")
} # }
```
