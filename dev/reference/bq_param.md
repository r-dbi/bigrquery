# Explicitly define query parameters

By default, bigrquery will assume vectors of length 1 are scalars, and
longer vectors are arrays. If you need to pass a length-1 array, you'll
need to explicitly use `bq_param_array()`.

## Usage

``` r
bq_param(value, type = NULL, name = NULL)

bq_param_scalar(value, type = NULL, name = NULL)

bq_param_array(value, type = NULL, name = NULL)
```

## Arguments

- value:

  vector of parameter values

- type:

  BigQuery type of the parameter

- name:

  name of the parameter in the query, omitting the `@`

## Examples

``` r
# bq_param() automatically picks scalar vs array based on length
bq_param("a")
#> {
#>   "name": {},
#>   "parameterType": {
#>     "type": "STRING"
#>   },
#>   "parameterValue": {
#>     "value": "a"
#>   }
#> }
bq_param(c("a", "b", "c"))
#> {
#>   "name": {},
#>   "parameterType": {
#>     "type": "ARRAY",
#>     "arrayType": {
#>       "type": "STRING"
#>     }
#>   },
#>   "parameterValue": {
#>     "arrayValues": [
#>       {
#>         "value": "a"
#>       },
#>       {
#>         "value": "b"
#>       },
#>       {
#>         "value": "c"
#>       }
#>     ]
#>   }
#> }

# use bq_param_array() to create a length-1 array
bq_param_array("a")
#> {
#>   "name": {},
#>   "parameterType": {
#>     "type": "ARRAY",
#>     "arrayType": {
#>       "type": "STRING"
#>     }
#>   },
#>   "parameterValue": {
#>     "arrayValues": [
#>       {
#>         "value": "a"
#>       }
#>     ]
#>   }
#> }
```
