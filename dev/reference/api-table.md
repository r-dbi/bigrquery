# BigQuery tables

Basic create-read-update-delete verbs for tables, as well as functions
uploading data (`bq_table_upload()`), saving to/loading from Google
Cloud Storage (`bq_table_load()`, `bq_table_save()`), and getting
various values from the metadata.

## Usage

``` r
bq_table_create(x, fields = NULL, ...)

bq_table_meta(x, fields = NULL)

bq_table_fields(x)

bq_table_size(x)

bq_table_nrow(x)

bq_table_exists(x)

bq_table_delete(x)

bq_table_copy(x, dest, ..., quiet = getOption("bigrquery.quiet", NA))

bq_table_upload(
  x,
  values,
  ...,
  api = c("json", "arrow"),
  quiet = getOption("bigrquery.quiet", NA),
  json_digits = NA
)

bq_table_save(
  x,
  destination_uris,
  ...,
  destination_format = "NEWLINE_DELIMITED_JSON",
  compression = "NONE",
  quiet = getOption("bigrquery.quiet", NA)
)

bq_table_load(x, source_uris, ..., quiet = getOption("bigrquery.quiet", NA))

bq_table_patch(x, fields)
```

## Arguments

- x:

  A [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md), or
  an object coercible to a `bq_table`.

- fields:

  A [bq_fields](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  specification, or something coercible to it (like a data frame).

- ...:

  Additional arguments passed on to the underlying API call. snake_case
  names are automatically converted to camelCase.

- dest:

  Source and destination
  [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)s.

- quiet:

  If `FALSE`, displays progress bar; if `TRUE` is silent; if `NA` picks
  based on whether or not you're in an interactive context.

- values:

  Data frame of values to insert.

- api:

  Which API to use? `"json"` uploads using a BigQuery load job
  ([`bq_perform_upload()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)),
  while `"arrow"` streams data via the BigQuery Storage Write API
  ([`bigrquerystorage::bqs_table_upload()`](https://rdrr.io/pkg/bigrquerystorage/man/bqs_table_upload.html)).
  If bigrquerystorage is installed, `api = "arrow"` is used by default.

- json_digits:

  Controls how doubles are formatted when uploading as JSON. Passed on
  to the `digits` argument of
  [`jsonlite::toJSON()`](https://rdrr.io/pkg/jsonlite/man/fromJSON.html):

  - `NA` (the default) uses 15 significant digits. This is the maximum
    number of digits that a double can reliably represent, and avoids
    floating point artefacts (e.g. `1.06` becoming `1.0600000000000001`)
    that `NUMERIC` columns reject.

  - A whole number uses at most that many digits after the decimal
    point. Use `22` to preserve every bit of a double, but note that the
    extra digits may cause uploads to `NUMERIC` columns to fail.

- destination_uris:

  A character vector of fully-qualified Google Cloud Storage URIs where
  the extracted table should be written. Can export up to 1 Gb of data
  per file. Use a wild card URI (e.g.
  `gs://[YOUR_BUCKET]/file-name-*.json`) to automatically create any
  number of files.

- destination_format:

  The exported file format:

  - For CSV files, specify "CSV" (Nested and repeated data is not
    supported).

  - For newline-delimited JSON, specify "NEWLINE_DELIMITED_JSON".

  - For Avro, specify "AVRO".

  - For parquet, specify "PARQUET".

- compression:

  The compression type to use for exported files:

  - For CSV files: "GZIP" or "NONE".

  - For newline-delimited JSON: "GZIP" or "NONE".

  - For Avro: "DEFLATE", "SNAPPY" or "NONE".

  - For parquet: "SNAPPY", "GZIP", "ZSTD" or "NONE".

- source_uris:

  The fully-qualified URIs that point to your data in Google Cloud.

  For Google Cloud Storage URIs: Each URI can contain one `'*'` wildcard
  character and it must come after the 'bucket' name. Size limits
  related to load jobs apply to external data sources.

  For Google Cloud Bigtable URIs: Exactly one URI can be specified and
  it has be a fully specified and valid HTTPS URL for a Google Cloud
  Bigtable table. For Google Cloud Datastore backups: Exactly one URI
  can be specified. Also, the '\*' wildcard character is not allowed.

## Value

- `bq_table_copy()`, `bq_table_create()`, `bq_table_delete()`,
  `bq_table_upload()`: an invisible
  [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)

- `bq_table_exists()`: either `TRUE` or `FALSE`.

- `bq_table_size()`: the size of the table in bytes

- `bq_table_fields()`: a
  [bq_fields](https://bigrquery.r-dbi.org/dev/reference/bq_field.md).

## Google BigQuery API documentation

- [insert](https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/tables/insert)

- [get](https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/tables/get)

- [delete](https://docs.cloud.google.com/bigquery/docs/reference/rest/v2/tables/delete)

## Examples

``` r
if (FALSE) { # bq_testable()
ds <- bq_test_dataset()

bq_mtcars <- bq_table(ds, "mtcars")
bq_table_exists(bq_mtcars)

bq_table_create(
  bq_mtcars,
  fields = mtcars,
  friendly_name = "Motor Trend Car Road Tests",
  description = "The data was extracted from the 1974 Motor Trend US magazine",
  labels = list(category = "example")
)
bq_table_exists(bq_mtcars)

bq_table_upload(bq_mtcars, mtcars)

bq_table_fields(bq_mtcars)
bq_table_size(bq_mtcars)
str(bq_table_meta(bq_mtcars))

bq_table_delete(bq_mtcars)
bq_table_exists(bq_mtcars)

my_natality <- bq_table(ds, "mynatality")
bq_table_copy("publicdata.samples.natality", my_natality)
}
```
