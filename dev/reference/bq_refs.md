# S3 classes for BigQuery datasets, tables and jobs

Create references to BigQuery datasets, jobs, and tables. Each class has
a constructor function (`bq_dataset()`, `bq_table()`, `bq_job()`) and a
coercion function (`as_bq_dataset()`, `as_bq_table()`, `as_bq_job()`).
The coercions functions come with methods for strings (which find
components by splitting on `.`), and lists (which look for named
components like `projectId` or `project_id`).

All `bq_table_`, `bq_dataset_` and `bq_job_` functions call the
appropriate coercion functions on their first argument, allowing you to
flexible specify their inputs.

## Usage

``` r
bq_dataset(project, dataset)

as_bq_dataset(x, ..., error_arg = caller_arg(x), error_call = caller_env())

bq_table(project, dataset, table = NULL, type = "TABLE")

as_bq_table(x, ..., error_arg = caller_arg(x), error_call = caller_env())

bq_job(project, job, location = "US")

as_bq_job(x, ..., error_arg = caller_arg(x), error_call = caller_env())
```

## Arguments

- project, dataset, table, job, type:

  Individual project, dataset, table, job identifiers and table type
  (strings).

  For `bq_table()`, you if supply a `bq_dataset` as the first argument,
  the 2nd argument will be interpreted as the `table`

- x:

  An object to coerce to a `bq_job`, `bq_dataset`, or `bq_table`.
  Built-in methods handle strings and lists.

- ...:

  Other arguments passed on to methods.

- error_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- location:

  Job location

## See also

[api-job](https://bigrquery.r-dbi.org/dev/reference/api-job.md),
[api-perform](https://bigrquery.r-dbi.org/dev/reference/api-perform.md),
[api-dataset](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md),
and [api-table](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
for functions that work with these objects.

## Examples

``` r
# Creation ------------------------------------------------
samples <- bq_dataset("publicdata", "samples")
natality <- bq_table("publicdata", "samples", "natality")
natality
#> <bq_table> publicdata.samples.natality

# Or
bq_table(samples, "natality")
#> <bq_table> publicdata.samples.natality

bq_job("bigrquery-examples", "m0SgFu2ycbbge6jgcvzvflBJ_Wft")
#> <bq_job> bigrquery-examples.m0SgFu2ycbbge6jgcvzvflBJ_Wft.US

# Coercion ------------------------------------------------
as_bq_dataset("publicdata.shakespeare")
#> <bq_dataset> publicdata.shakespeare
as_bq_table("publicdata.samples.natality")
#> <bq_table> publicdata.samples.natality

as_bq_table(list(
  project_id = "publicdata",
  dataset_id = "samples",
  table_id = "natality"
))
#> <bq_table> publicdata.samples.natality

as_bq_job(list(
  projectId = "bigrquery-examples",
  jobId = "job_m0SgFu2ycbbge6jgcvzvflBJ_Wft",
  location = "US"
))
#> <bq_job> bigrquery-examples.job_m0SgFu2ycbbge6jgcvzvflBJ_Wft.US
```
