# Clear current token

Clears any currently stored token. The next time bigrquery needs a
token, the token acquisition process starts over, with a fresh call to
[`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md) and,
therefore, internally, a call to
[`gargle::token_fetch()`](https://gargle.r-lib.org/reference/token_fetch.html).
Unlike some other packages that use gargle, bigrquery is not usable in a
de-authorized state. Therefore, calling `bq_deauth()` only clears the
token, i.e. it does NOT imply that subsequent requests are made with an
API key in lieu of a token.

## Usage

``` r
bq_deauth()
```

## See also

Other auth functions:
[`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md),
[`bq_auth_configure()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bq_deauth()
} # }
```
