# Produce configured token

For internal use or for those programming around the BigQuery API.
Returns a token pre-processed with
[`httr::config()`](https://httr.r-lib.org/reference/config.html). Most
users do not need to handle tokens "by hand" or, even if they need some
control,
[`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md) is
what they need. If there is no current token,
[`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md) is
called to either load from cache or initiate OAuth2.0 flow. If auth has
been deactivated via
[`bq_deauth()`](https://bigrquery.r-dbi.org/dev/reference/bq_deauth.md),
`bq_token()` returns `NULL`.

## Usage

``` r
bq_token()
```

## Value

A `request` object (an S3 class provided by
[httr](https://httr.r-lib.org/reference/httr-package.html)).

## See also

Other low-level API functions:
[`bq_has_token()`](https://bigrquery.r-dbi.org/dev/reference/bq_has_token.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bq_token()
} # }
```
