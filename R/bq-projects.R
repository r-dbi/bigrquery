#' List available projects
#'
#' List all projects that you have access to. You can also work with
#' [public datasets](https://cloud.google.com/bigquery/public-data/),
#' but you will need to provide a `billing` project whenever you perform
#' any non-free operation.
#'
#' @section API documentation:
#' * [list](https://cloud.google.com/bigquery/docs/reference/rest/v2/projects/list)
#' @param page_size Number of items per page.
#' @param max_pages Maximum number of pages to retrieve. Use `Inf` to retrieve
#'   all pages (this may take a long time!)
#' @param warn If `TRUE`, warn when there are unretrieved pages.
#' @return A character vector.
#' @export
#' @examples
#' if (bq_authable()) {
#' bq_projects()
#' }
bq_projects <- function(page_size = 100, max_pages = 1, warn = TRUE) {
  pages <- bq_get_paginated(
    "projects",
    query = list(fields = "projects(projectReference(projectId))"),
    page_size = page_size,
    max_pages = max_pages
  )
  projects <- unlist(lapply(pages, function(x) x$projects), recursive = FALSE)
  map_chr(projects, function(x) x$projectReference$projectId)
}
