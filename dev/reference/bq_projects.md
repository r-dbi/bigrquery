# List available projects

List all projects that you have access to. You can also work with
[public datasets](https://cloud.google.com/bigquery/public-data/), but
you will need to provide a `billing` project whenever you perform any
non-free operation.

## Usage

``` r
bq_projects(page_size = 100, max_pages = 1, warn = TRUE)
```

## Arguments

- page_size:

  Number of items per page.

- max_pages:

  Maximum number of pages to retrieve. Use `Inf` to retrieve all pages
  (this may take a long time!)

- warn:

  If `TRUE`, warn when there are unretrieved pages.

## Value

A character vector.

## Google BigQuery API documentation

- [list](https://cloud.google.com/bigquery/docs/reference/rest/v2/projects/list)

## Examples

``` r
if (FALSE) { # bq_testable()
bq_projects()
}
```
