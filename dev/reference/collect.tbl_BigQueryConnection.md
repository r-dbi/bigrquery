# Collect a BigQuery table

This collect method is specialised for BigQuery tables, generating the
SQL from your dplyr commands, then calling
[`bq_project_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)
or
[`bq_dataset_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)
to run the query, then
[`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
to download the results. Thus the arguments are a combination of the
arguments to
[`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html),
[`bq_project_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)/[`bq_dataset_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md),
and
[`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md).

## Usage

``` r
collect.tbl_BigQueryConnection(
  x,
  ...,
  n = Inf,
  api = c("json", "arrow"),
  page_size = NULL,
  max_connections = 6L
)
```

## Arguments

- x:

  A data frame, data frame extension (e.g. a tibble), or a lazy data
  frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for more
  details.

- ...:

  Other arguments passed on to
  [`bq_project_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)/[`bq_project_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)

- n:

  Maximum number of results to retrieve. The default, `Inf`, will
  retrieve all rows.

- api:

  Which API to use? The `"json"` API works where ever bigrquery does,
  but is slow and can require fiddling with the `page_size` parameter.
  The `"arrow"` API is faster and more reliable, but only works if you
  have also installed the bigrquerystorage package.

  Because the `"arrow"` API is so much faster, it will be used
  automatically if the bigrquerystorage package is installed.

- page_size:

  (JSON only) The number of rows requested per chunk. It is recommended
  to leave this unspecified until you have evidence that the `page_size`
  selected automatically by
  [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  is problematic.

  When `page_size = NULL` bigrquery determines a conservative, natural
  chunk size empirically. If you specify the `page_size`, it is
  important that each chunk fits on one page, i.e. that the requested
  row limit is low enough to prevent the API from paginating based on
  response size.

- max_connections:

  (JSON only) Number of maximum simultaneous connections to BigQuery
  servers.
