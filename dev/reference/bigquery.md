# BigQuery DBI driver

Creates a BigQuery DBI driver for use in
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

## Usage

``` r
# S4 method for class 'BigQueryDriver'
dbConnect(
  drv,
  project,
  dataset = NULL,
  billing = project,
  page_size = 10000,
  quiet = NA,
  use_legacy_sql = FALSE,
  bigint = c("integer", "integer64", "numeric", "character"),
  ...
)
```

## Arguments

- drv:

  The result of `bigquery()`.

- project, dataset:

  Project and dataset identifiers

- billing:

  Identifier of project to bill.

- page_size:

  Number of items per page.

- quiet:

  If `FALSE`, displays progress bar; if `TRUE` is silent; if `NA` picks
  based on whether or not you're in an interactive context.

- use_legacy_sql:

  If `TRUE` will use BigQuery's legacy SQL format.

- bigint:

  The R type that BigQuery's 64-bit integer types should be mapped to.
  The default is `"integer"` which returns R's `integer` type but
  results in `NA` for values above/below +/- 2147483647. `"integer64"`
  returns a
  [bit64::integer64](https://rdrr.io/pkg/bit64/man/bit64-package.html),
  which allows the full range of 64 bit integers.

- ...:

  Other arguments for compatibility with generic; currently ignored.

## Examples

``` r
if (FALSE) { # bq_testable()
con <- DBI::dbConnect(
  bigquery(),
  project = "publicdata",
  dataset = "samples",
  billing = bq_test_project()
)
con
DBI::dbListTables(con)
DBI::dbReadTable(con, "natality", n_max = 10)

# Create a temporary dataset to explore
ds <- bq_test_dataset()
con <- DBI::dbConnect(
  bigquery(),
  project = ds$project,
  dataset = ds$dataset
)
DBI::dbWriteTable(con, "mtcars", mtcars)
DBI::dbReadTable(con, "mtcars")[1:6, ]

DBI::dbGetQuery(con, "SELECT count(*) FROM mtcars")

res <- DBI::dbSendQuery(con, "SELECT cyl, mpg FROM mtcars")
dbColumnInfo(res)
dbFetch(res, 10)
dbFetch(res, -1)
DBI::dbHasCompleted(res)
}
```
