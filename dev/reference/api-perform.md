# BigQuery jobs: perform a job

These functions are low-level functions designed to be used by experts.
Each of these low-level functions is paired with a high-level function
that you should use instead:

- `bq_perform_copy()`:
  [`bq_table_copy()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md).

- `bq_perform_query()`:
  [`bq_dataset_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md),
  [`bq_project_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md).

- `bq_perform_upload()`:
  [`bq_table_upload()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md).

- `bq_perform_load()`:
  [`bq_table_load()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md).

- `bq_perform_extract()`:
  [`bq_table_save()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md).

## Usage

``` r
bq_perform_extract(
  x,
  destination_uris,
  destination_format = "NEWLINE_DELIMITED_JSON",
  compression = "NONE",
  ...,
  print_header = TRUE,
  billing = x$project
)

bq_perform_upload(
  x,
  values,
  fields = NULL,
  source_format = c("NEWLINE_DELIMITED_JSON", "PARQUET"),
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_EMPTY",
  ...,
  billing = x$project
)

bq_perform_load(
  x,
  source_uris,
  billing = x$project,
  source_format = "NEWLINE_DELIMITED_JSON",
  fields = NULL,
  nskip = 0,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_EMPTY",
  ...
)

bq_perform_query(
  query,
  billing,
  ...,
  parameters = NULL,
  destination_table = NULL,
  default_dataset = NULL,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_EMPTY",
  use_legacy_sql = FALSE,
  priority = "INTERACTIVE"
)

bq_perform_query_dry_run(
  query,
  billing,
  ...,
  default_dataset = NULL,
  parameters = NULL,
  use_legacy_sql = FALSE
)

bq_perform_query_schema(
  query,
  billing,
  ...,
  default_dataset = NULL,
  parameters = NULL
)

bq_perform_copy(
  src,
  dest,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_EMPTY",
  ...,
  billing = NULL
)
```

## Arguments

- x:

  A [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)

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

- ...:

  Additional arguments passed on to the underlying API call. snake_case
  names are automatically converted to camelCase.

- print_header:

  Whether to print out a header row in the results.

- billing:

  Identifier of project to bill.

- values:

  Data frame of values to insert.

- fields:

  A [bq_fields](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  specification, or something coercible to it (like a data frame). Leave
  as `NULL` to allow BigQuery to auto-detect the fields.

- source_format:

  The format of the data files:

  - For CSV files, specify "CSV".

  - For datastore backups, specify "DATASTORE_BACKUP".

  - For newline-delimited JSON, specify "NEWLINE_DELIMITED_JSON".

  - For Avro, specify "AVRO".

  - For parquet, specify "PARQUET".

  - For orc, specify "ORC".

- create_disposition:

  Specifies whether the job is allowed to create new tables.

  The following values are supported:

  - "CREATE_IF_NEEDED": If the table does not exist, BigQuery creates
    the table.

  - "CREATE_NEVER": The table must already exist. If it does not, a
    'notFound' error is returned in the job result.

- write_disposition:

  Specifies the action that occurs if the destination table already
  exists. The following values are supported:

  - "WRITE_TRUNCATE": If the table already exists, BigQuery overwrites
    the table data.

  - "WRITE_APPEND": If the table already exists, BigQuery appends the
    data to the table.

  - "WRITE_EMPTY": If the table already exists and contains data, a
    'duplicate' error is returned in the job result.

- source_uris:

  The fully-qualified URIs that point to your data in Google Cloud.

  For Google Cloud Storage URIs: Each URI can contain one `'*'` wildcard
  character and it must come after the 'bucket' name. Size limits
  related to load jobs apply to external data sources.

  For Google Cloud Bigtable URIs: Exactly one URI can be specified and
  it has be a fully specified and valid HTTPS URL for a Google Cloud
  Bigtable table. For Google Cloud Datastore backups: Exactly one URI
  can be specified. Also, the '\*' wildcard character is not allowed.

- nskip:

  For `source_format = "CSV"`, the number of header rows to skip.

- query:

  SQL query string.

- parameters:

  Named list of parameters match to query parameters. Parameter `x` will
  be matched to placeholder `@x`.

  Generally, you can supply R vectors and they will be automatically
  converted to the correct type. If you need greater control, you can
  call
  [`bq_param_scalar()`](https://bigrquery.r-dbi.org/dev/reference/bq_param.md)
  or
  [`bq_param_array()`](https://bigrquery.r-dbi.org/dev/reference/bq_param.md)
  explicitly.

  See <https://cloud.google.com/bigquery/docs/parameterized-queries> for
  more details.

- destination_table:

  A [bq_table](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  where results should be stored. If not supplied, results will be saved
  to a temporary table that lives in a special dataset. You must supply
  this parameter for large queries (\> 128 MB compressed).

- default_dataset:

  A [bq_dataset](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  used to automatically qualify table names.

- use_legacy_sql:

  If `TRUE` will use BigQuery's legacy SQL format.

- priority:

  Specifies a priority for the query. Possible values include
  "INTERACTIVE" and "BATCH". Batch queries do not start immediately, but
  are not rate-limited in the same way as interactive queries.

## Value

A [bq_job](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md).

## Google BigQuery API documentation

- [jobs](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs)

Additional information at:

- [exporting
  data](https://cloud.google.com/bigquery/docs/exporting-data)

- [loading data](https://cloud.google.com/bigquery/docs/loading-data)

- [writing
  queries](https://cloud.google.com/bigquery/docs/writing-results)

- [copying a
  table](https://cloud.google.com/bigquery/docs/managing-tables#copy-table)

## Examples

``` r
if (FALSE) { # bq_testable()
ds <- bq_test_dataset()
bq_mtcars <- bq_table(ds, "mtcars")
job <- bq_perform_upload(bq_mtcars, mtcars)
bq_table_exists(bq_mtcars)

bq_job_wait(job)
bq_table_exists(bq_mtcars)
head(bq_table_download(bq_mtcars))
}
```
