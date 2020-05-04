#' List datasets [deprecated]
#'
#' Please use [api-project] instead.
#'
#' @keywords internal
#' @export
list_datasets <- function(project, page_size = 50, max_pages = Inf) {

  .Deprecated("bq_project_datasets", package = "bigrquery")

  assert_that(is.string(project))

  pages <- bq_get_paginated(
    bq_path(project, ""),
    page_size = page_size,
    max_pages = max_pages
  )

  datasets <- unlist(lapply(pages, function(x) x$datasets), recursive = FALSE)
  vapply(datasets, function(x) x$datasetReference$datasetId, character(1))
}
