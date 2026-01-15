# BigQuery job: retrieve metadata

To perform a job, see
[api-perform](https://bigrquery.r-dbi.org/dev/reference/api-perform.md).
These functions all retrieve metadata (in various forms) about an
existing job.

## Usage

``` r
bq_job_meta(x, fields = NULL)

bq_job_status(x)

bq_job_show_statistics(x)

bq_job_wait(
  x,
  quiet = getOption("bigrquery.quiet"),
  pause = 0.5,
  call = caller_env()
)
```

## Arguments

- x:

  A [bq_job](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)

- fields:

  An optional field specification for [partial
  response](https://cloud.google.com/bigquery/docs/api-performance#partial-response)

- quiet:

  If `FALSE`, displays progress bar; if `TRUE` is silent; if `NA` picks
  based on whether or not you're in an interactive context.

- pause:

  amount of time to wait between status requests

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Google BigQuery API documentation

- [get](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/get)

## Examples

``` r
if (FALSE) { # bq_testable()
jobs <- bq_project_jobs(bq_test_project())
jobs[[1]]

# Show statistics about job
bq_job_show_statistics(jobs[[1]])

# Wait for job to complete
bq_job_wait(jobs[[1]])
}
```
