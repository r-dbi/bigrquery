# BigQuery datasets

Basic create-read-update-delete verbs for datasets.

## Usage

``` r
bq_dataset_create(x, location = "US", ...)

bq_dataset_meta(x, fields = NULL)

bq_dataset_exists(x)

bq_dataset_update(x, ...)

bq_dataset_delete(x, delete_contents = FALSE)

bq_dataset_tables(x, page_size = 50, max_pages = Inf, warn = TRUE, ...)
```

## Arguments

- x:

  A [bq_dataset](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)

- location:

  Dataset location

- ...:

  Additional arguments passed on to the underlying API call. snake_case
  names are automatically converted to camelCase.

- fields:

  An optional field specification for [partial
  response](https://cloud.google.com/bigquery/docs/api-performance#partial-response)

- delete_contents:

  If `TRUE`, will recursively delete all tables in the dataset. Set to
  `FALSE` by default for safety.

- page_size:

  Number of items per page.

- max_pages:

  Maximum number of pages to retrieve. Use `Inf` to retrieve all pages
  (this may take a long time!)

- warn:

  If `TRUE`, warn when there are unretrieved pages.

## Google BigQuery API documentation

- [get](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/get)

- [insert](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/insert)

- [delete](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/delete)

- [list](https://cloud.google.com/bigquery/docs/reference/rest/v2/tables/list)

## Examples

``` r
if (FALSE) { # bq_testable()
ds <- bq_dataset(bq_test_project(), "dataset_api")
bq_dataset_exists(ds)

bq_dataset_create(ds)
bq_dataset_exists(ds)
str(bq_dataset_meta(ds))

bq_dataset_delete(ds)
bq_dataset_exists(ds)

# Use bq_test_dataset() to create a temporary dataset that will
# be automatically deleted
ds <- bq_test_dataset()
bq_table_create(bq_table(ds, "x1"))
bq_table_create(bq_table(ds, "x2"))
bq_table_create(bq_table(ds, "x3"))
bq_dataset_tables(ds)
}
```
