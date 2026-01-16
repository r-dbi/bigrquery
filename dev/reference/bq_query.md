# Submit query to BigQuery

These submit a query (using
[`bq_perform_query()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md))
and then wait for it complete (with
[`bq_job_wait()`](https://bigrquery.r-dbi.org/dev/reference/api-job.md)).
All BigQuery queries save their results into a table (temporary or
otherwise), so these functions return a
[bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md) which
you can then query for more information.

## Usage

``` r
bq_project_query(
  x,
  query,
  destination_table = NULL,
  ...,
  quiet = getOption("bigrquery.quiet", NA)
)

bq_dataset_query(
  x,
  query,
  destination_table = NULL,
  ...,
  billing = NULL,
  quiet = getOption("bigrquery.quiet", NA)
)
```

## Arguments

- x:

  Either a project (a string) or a
  [bq_dataset](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md).

- query:

  SQL query string.

- destination_table:

  A [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  where results should be stored. If not supplied, results will be saved
  to a temporary table that lives in a special dataset. You must supply
  this parameter for large queries (\> 128 MB compressed).

- ...:

  Passed on to
  [`bq_perform_query()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)

- quiet:

  If `FALSE`, displays progress bar; if `TRUE` is silent; if `NA` picks
  based on whether or not you're in an interactive context.

- billing:

  If you query a dataset that you only have read access for, such as a
  public dataset, you must also submit a `billing` project.

## Value

A [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)

## Examples

``` r
if (FALSE) { # bq_testable()
# Querying a project requires full name in query
tb <- bq_project_query(
  bq_test_project(),
  "SELECT count(*) FROM publicdata.samples.natality"
)
bq_table_fields(tb)
bq_table_download(tb)

# Querying a dataset sets default dataset so you can use bare table name,
# but for public data, you'll need to set a project to bill.
ds <- bq_dataset("publicdata", "samples")
tb <- bq_dataset_query(ds,
  query = "SELECT count(*) FROM natality",
  billing = bq_test_project()
)
bq_table_download(tb)

tb <- bq_dataset_query(ds,
  query = "SELECT count(*) FROM natality WHERE state = @state",
  parameters = list(state = "KS"),
  billing = bq_test_project()
)
bq_table_download(tb)
}
```
