# Edit and view auth configuration

These functions give more control over and visibility into the auth
configuration than
[`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md)
does. `bq_auth_configure()` lets the user specify their own:

- OAuth client, which is used when obtaining a user token.

See the
[`vignette("get-api-credentials", package = "gargle")`](https://gargle.r-lib.org/articles/get-api-credentials.html)
for more. If the user does not configure these settings, internal
defaults are used.

`bq_oauth_client()` retrieves the currently configured OAuth client.

## Usage

``` r
bq_auth_configure(client, path)

bq_oauth_client()
```

## Arguments

- client:

  A Google OAuth client, presumably constructed via
  [`gargle::gargle_oauth_client_from_json()`](https://gargle.r-lib.org/reference/gargle_oauth_client_from_json.html).
  Note, however, that it is preferred to specify the client with JSON,
  using the `path` argument.

- path:

  JSON downloaded from [Google Cloud
  Console](https://console.cloud.google.com), containing a client id and
  secret, in one of the forms supported for the `txt` argument of
  [`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
  (typically, a file path or JSON string).

## Value

- `bq_auth_configure()`: An object of R6 class
  [gargle::AuthState](https://gargle.r-lib.org/reference/AuthState-class.html),
  invisibly.

- `bq_oauth_client()`: the current user-configured OAuth client.

## See also

Other auth functions:
[`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md),
[`bq_deauth()`](https://bigrquery.r-dbi.org/dev/reference/bq_deauth.md)

## Examples

``` r
# see and store the current user-configured OAuth client (probably `NULL`)
(original_client <- bq_oauth_client())
#> NULL

# the preferred way to configure your own client is via a JSON file
# downloaded from Google Developers Console
# this example JSON is indicative, but fake
path_to_json <- system.file(
  "extdata", "data", "client_secret_123.googleusercontent.com.json",
  package = "bigrquery"
)
bq_auth_configure(path = path_to_json)

# confirm the changes
bq_oauth_client()
#> <gargle_oauth_client>
#> name: a_project_d1c5a8066d2cbe48e8d94514dd286163
#> id: abc.apps.googleusercontent.com
#> secret: <REDACTED>
#> type: installed
#> redirect_uris: http://localhost

# restore original auth config
bq_auth_configure(client = original_client)
```
