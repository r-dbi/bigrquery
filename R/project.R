
#' List the datasets in a project
#'
#' @return a character vector of dataset names
#' @param project A project identifier
#' @inheritParams bq_dataset_tables
#' @seealso [API documentation](https://developers.google.com/bigquery/docs/reference/v2/datasets/list)
#' @family datasets
#' @export
#' @examples
#' if (bq_authable()) {
#' bq_project_datasets("publicdata")
#' bq_project_datasets("githubarchive")
#' }
bq_project_datasets <- function(project, page_size = 50, max_pages = Inf) {
  assert_that(is.string(project))

  pages <- bq_get_paginated(
    bq_path(project, ""),
    page_size = page_size,
    max_pages = max_pages
  )

  datasets <- unlist(lapply(pages, function(x) x$datasets), recursive = FALSE)
  lapply(datasets, function(x) {
    ref <- x$datasetReference
    bq_dataset(ref$projectId, ref$datasetId)
  })
}

#' @rdname bq_project_datasets
#' @export
#' @usage NULL
list_datasets <- function(project, page_size = 50, max_pages = Inf) {
  assert_that(is.string(project))

  pages <- bq_get_paginated(
    bq_path(project, ""),
    page_size = page_size,
    max_pages = max_pages
  )

  datasets <- unlist(lapply(pages, function(x) x$datasets), recursive = FALSE)
  vapply(datasets, function(x) x$datasetReference$datasetId, character(1))
}
