# BigQuery project methods

Projects have two primary components: datasets and jobs. Unlike other
BigQuery objects, is no accompanying `bq_project` S3 class because a
project is a simple string.

## Usage

``` r
bq_project_datasets(x, page_size = 100, max_pages = 1, warn = TRUE)

bq_project_jobs(x, page_size = 100, max_pages = 1, warn = TRUE)
```

## Arguments

- x:

  A string giving a project name.

- page_size:

  Number of items per page.

- max_pages:

  Maximum number of pages to retrieve. Use `Inf` to retrieve all pages
  (this may take a long time!)

- warn:

  If `TRUE`, warn when there are unretrieved pages.

## Value

- `bq_project_datasets()`: a list of
  [bq_dataset](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)s

- `bq_project_jobs()`: a list of
  [bq_job](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)s.

## Google BigQuery API documentation

- [datasets](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/list)

- [jobs](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/list)

One day we might also expose the general [project
metadata](https://cloud.google.com/resource-manager/reference/rest/v1/projects).

## Examples

``` r
if (FALSE) { # bq_testable()
bq_project_datasets("bigquery-public-data")
bq_project_datasets("githubarchive")

bq_project_jobs(bq_test_project(), page_size = 10)
}
```
