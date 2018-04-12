#' BigQuery project methods
#'
#' Projects have two primary components: datasets and jobs. Unlike other
#' BigQuery objects, is no accompanying `bq_project` S3 class because a project
#' is a simple string.
#'
#' @section API documentation:
#' * [datasets](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/list)
#' * [jobs](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/list)
#'
#' One day we might also expose the general [project metadata](https://cloud.google.com/resource-manager/reference/rest/v1/projects).
#'
#' @return
#' * `bq_project_datasets()`: a list of [bq_dataset]s
#' * `bq_project_jobs()`: a list of [bq_job]s.
#'
#' @name api-project
#' @examples
#' if (bq_authable()) {
#' bq_project_datasets("bigquery-public-data")
#' bq_project_datasets("githubarchive")
#' }
#'
#' if (bq_testable()) {
#' bq_project_jobs(bq_test_project(), page_size = 10)
#' }
NULL

#' @export
#' @rdname api-project
#' @param x A string giving a project name.
#' @inheritParams bq_projects
bq_project_datasets <- function(x, page_size = 100, max_pages = 1, warn = TRUE) {
  assert_that(is.string(x))

  pages <- bq_get_paginated(
    bq_path(x, ""),
    query = list(fields = "datasets(datasetReference)"),
    page_size = page_size,
    max_pages = max_pages,
    warn = warn
  )

  datasets <- unlist(lapply(pages, function(x) x$datasets), recursive = FALSE)
  lapply(datasets, function(x) {
    ref <- x$datasetReference
    bq_dataset(ref$projectId, ref$datasetId)
  })
}

#' @export
#' @rdname api-project
bq_project_jobs <- function(x, page_size = 100, max_pages = 1, warn = TRUE) {
  assert_that(is.string(x))

  pages <- bq_get_paginated(
    bq_path(x, jobs = ""),
    query = list(fields = "jobs(jobReference)"),
    page_size = page_size,
    max_pages = max_pages,
    warn = warn
  )
  jobs <- unlist(lapply(pages, function(x) x$jobs), recursive = FALSE)
  lapply(jobs, function(x) as_bq_job(x$jobReference))
}
