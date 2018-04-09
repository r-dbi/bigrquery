#' Project operations
#'
#' @section API documentation:
#' * [datasets](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/list)
#' * [jobs](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/list)
#'
#' One day we might also expose the general [project metadata]
#' (https://cloud.google.com/resource-manager/reference/rest/v1/projects).
#'
#' @return:
#' * `bq_project_dataset()`: a list of [bq_dataset]s
#' * `bq_project_jobs()`: a list of [bq_jobs]s.
#' * `bq_project_query()`: a [bq_table].
#'
#' @name project-API
#' @examples
#' if (bq_authable()) {
#' bq_project_datasets("bigquery-public-data")
#' bq_project_datasets("githubarchive")
#' }
NULL

#' @export
#' @rdname project-API
bq_project_datasets <- function(project, page_size = 50, max_pages = 1) {
  assert_that(is.string(project))

  pages <- bq_get_paginated(
    bq_path(project, ""),
    query = list(fields = "datasets(datasetReference)"),
    page_size = page_size,
    max_pages = max_pages
  )

  datasets <- unlist(lapply(pages, function(x) x$datasets), recursive = FALSE)
  lapply(datasets, function(x) {
    ref <- x$datasetReference
    bq_dataset(ref$projectId, ref$datasetId)
  })
}

#' @export
#' @rdname project-API
bq_project_jobs <- function(project, page_size = 50, max_pages = 1) {
  assert_that(is.string(project))

  pages <- bq_get_paginated(
    bq_path(project, jobs = ""),
    query = list(fields = "jobs(jobReference)"),
    page_size = page_size,
    max_pages = max_pages
  )
  jobs <- unlist(lapply(pages, function(x) x$jobs), recursive = FALSE)
  lapply(jobs, function(x) {
    ref <- x$jobReference
    bq_job(ref$projectId, ref$jobId)
  })
}
