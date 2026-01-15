# Project to use for testing bigrquery

You'll need to set the `BIGQUERY_TEST_PROJECT` (name of a project) and
`BIGQUERY_TEST_BUCKET` (name of bucket) env vars in order to run
bigrquery tests locally. I recommend creating a new project because the
tests involve both reading and writing in BigQuery and Cloud Storage.

The `BIGQUERY_TEST_PROJECT` must have billing enabled for the project.
While logged in, via
[`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md), as
a user with permission to work in `BIGQUERY_TEST_PROJECT`, run
`bq_test_init()` once to perform some setup.

## Usage

``` r
bq_test_project()

bq_test_init(name = "basedata")

bq_test_dataset(name = random_name(), location = "US")

bq_testable()

bq_authable()

gs_test_bucket()

gs_test_object(name = random_name())
```

## Arguments

- name:

  Dataset name - used only for testing.

## Value

`bq_test_project()` returns the name of a project suitable for use in
testing. `bq_test_dataset()` creates a temporary dataset whose lifetime
is tied to the lifetime of the object that it returns.

## Testing

In tests, `bq_test_project()` (and hence `bq_test_dataset()`) will
automatically skip if auth and a test project are not available.

## Examples

``` r
if (FALSE) { # bq_testable()
ds <- bq_test_dataset()
bq_mtcars <- bq_table_upload(bq_table(ds, "mtcars"), mtcars)

# dataset and table will be automatically deleted when ds is GC'd
}
```
