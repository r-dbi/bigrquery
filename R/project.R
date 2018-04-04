
#' List the datasets in a project
#'
#' @return a character vector of dataset names
#' @inheritParams list_tables
#' @seealso Google API documentation:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/datasets/list}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' list_datasets("publicdata")
#' list_datasets("githubarchive")
#' }
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
