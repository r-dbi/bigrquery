# Is there a token on hand?

Reports whether bigrquery has stored a token, ready for use in
downstream requests.

## Usage

``` r
bq_has_token()
```

## Value

Logical.

## See also

Other low-level API functions:
[`bq_token()`](https://bigrquery.r-dbi.org/dev/reference/bq_token.md)

## Examples

``` r
bq_has_token()
#> [1] FALSE
```
