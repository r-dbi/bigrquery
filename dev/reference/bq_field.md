# BigQuery field (and fields) class

`bq_field()` and `bq_fields()` create; `as_bq_field()` and
`as_bq_fields()` coerce from lists.

## Usage

``` r
bq_field(name, type, mode = "NULLABLE", fields = list(), description = NULL)

bq_fields(x)

as_bq_field(x)

as_bq_fields(x)
```

## Arguments

- name:

  The field name. The name must contain only letters (a-z, A-Z), numbers
  (0-9), or underscores (\_), and must start with a letter or
  underscore. The maximum length is 300 characters.

- type:

  The field data type. Possible values include: `"STRING"`, `"BYTES"`,
  `"INTEGER"`, `"FLOAT"`, `"BOOLEAN"`, `"TIMESTAMP"`, `"DATE"`,
  `"TIME"`, `"DATETIME"`, `"GEOGRAPHY"`, `"NUMERIC"`, `"BIGNUMERIC"`,
  `"JSON"`, `"RECORD"`.

- mode:

  The field mode. Possible values include: `"NULLABLE"`, `"REQUIRED"`,
  and `"REPEATED"`.

- fields:

  For a field of type "record", a list of sub-fields.

- description:

  The field description. The maximum length is 1,024 characters.

- x:

  A list of `bg_fields`

## See also

`bq_field()` corresponds to a `TableFieldSchema`, see
<https://cloud.google.com/bigquery/docs/reference/rest/v2/tables#TableFieldSchema>
for more details.

## Examples

``` r
bq_field("name", "string")
#> <bq_field> name <STRING>
#> 

as_bq_fields(list(
  list(name = "name", type = "string"),
  bq_field("age", "integer")
))
#> <bq_fields>
#>   name <STRING>
#>   age <INTEGER>
#> 

# as_bq_fields() can also take a data frame
as_bq_fields(mtcars)
#> <bq_fields>
#>   mpg <FLOAT>
#>   cyl <FLOAT>
#>   disp <FLOAT>
#>   hp <FLOAT>
#>   drat <FLOAT>
#>   wt <FLOAT>
#>   qsec <FLOAT>
#>   vs <FLOAT>
#>   am <FLOAT>
#>   gear <FLOAT>
#>   carb <FLOAT>
#> 
```
