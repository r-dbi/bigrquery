# Download table data

This function provides two ways to download data from BigQuery,
transfering data using either JSON or arrow, depending on the `api`
argument. If bigrquerystorage is installed, `api = "arrow"` will be used
(because it's so much faster, but see the limitions below), otherwise
you can select deliberately by using `api = "json"` or `api = "arrow"`.

### Arrow API

The arrow API is much faster, but has heavier dependencies:
bigrquerystorage requires the arrow package, which can be tricky to
compile on Linux (but you usually should be able to get a binary from
[Posit Public Package Manager](https://p3m.dev/).

There's one known limitation of `api = "arrow"`: when querying public
data, you'll now need to provide a `billing` project.

### JSON API

The JSON API retrieves rows in chunks of `page_size`. It is most
suitable for results of smaller queries (\<100 MB, say). Unfortunately
due to limitations in the BigQuery API, you may need to vary this
parameter depending on the complexity of the underlying data.

The JSON API will convert nested and repeated columns in to list-columns
as follows:

- Repeated values (arrays) will become a list-column of vectors.

- Records will become list-columns of named lists.

- Repeated records will become list-columns of data frames.

## Usage

``` r
bq_table_download(
  x,
  n_max = Inf,
  page_size = NULL,
  start_index = 0L,
  max_connections = 6L,
  quiet = getOption("bigrquery.quiet", NA),
  bigint = c("integer", "integer64", "numeric", "character"),
  api = c("json", "arrow"),
  billing = x$project
)
```

## Arguments

- x:

  A [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)

- n_max:

  Maximum number of results to retrieve. Use `Inf` to retrieve all rows.

- page_size:

  (JSON only) The number of rows requested per chunk. It is recommended
  to leave this unspecified until you have evidence that the `page_size`
  selected automatically by `bq_table_download()` is problematic.

  When `page_size = NULL` bigrquery determines a conservative, natural
  chunk size empirically. If you specify the `page_size`, it is
  important that each chunk fits on one page, i.e. that the requested
  row limit is low enough to prevent the API from paginating based on
  response size.

- start_index:

  (JSON only) Starting row index (zero-based).

- max_connections:

  (JSON only) Number of maximum simultaneous connections to BigQuery
  servers.

- quiet:

  If `FALSE`, displays progress bar; if `TRUE` is silent; if `NA` picks
  based on whether or not you're in an interactive context.

- bigint:

  The R type that BigQuery's 64-bit integer types should be mapped to.
  The default is `"integer"`, which returns R's `integer` type, but
  results in `NA` for values above/below +/- 2147483647. `"integer64"`
  returns a
  [bit64::integer64](https://rdrr.io/pkg/bit64/man/bit64-package.html),
  which allows the full range of 64 bit integers.

- api:

  Which API to use? The `"json"` API works where ever bigrquery does,
  but is slow and can require fiddling with the `page_size` parameter.
  The `"arrow"` API is faster and more reliable, but only works if you
  have also installed the bigrquerystorage package.

  Because the `"arrow"` API is so much faster, it will be used
  automatically if the bigrquerystorage package is installed.

- billing:

  (Arrow only) Project to bill; defaults to the project of `x`, and
  typically only needs to be specified if you're working with public
  datasets.

## Value

Because data retrieval may generate list-columns and the `data.frame`
print method can have problems with list-columns, this method returns a
tibble. If you need a `data.frame`, coerce the results with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Google BigQuery API documentation

- [list](https://cloud.google.com/bigquery/docs/reference/rest/v2/tabledata/list)

## Examples

``` r
if (FALSE) { # bq_testable()
df <- bq_table_download("publicdata.samples.natality", n_max = 35000, billing = bq_test_project())
}
```
