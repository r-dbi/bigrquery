# Get info on current user

Reveals the email address of the user associated with the current token.
If no token has been loaded yet, this function does not initiate auth.

## Usage

``` r
bq_user()
```

## Value

An email address or, if no token has been loaded, `NULL`.

## See also

[`gargle::token_userinfo()`](https://gargle.r-lib.org/reference/token-info.html),
[`gargle::token_email()`](https://gargle.r-lib.org/reference/token-info.html),
[`gargle::token_tokeninfo()`](https://gargle.r-lib.org/reference/token-info.html)

## Examples

``` r
if (FALSE) { # \dontrun{
bq_user()
} # }
```
