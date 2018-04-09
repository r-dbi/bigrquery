#' Projects API
#'
#' There is currently only one project method which returns all projects to
#' which you have been granted access. You can also work with
#' [public datasets](https://cloud.google.com/bigquery/public-data/),
#' but you will need to provide a `billing` project whenever you perform
#' any non-free operation.
#'
#' There is no accompanying `bq_project` object because a project is a simple
#' string.
#'
#' @return A character vector of project ids named with their friendly name
#'   (if set).
#' @seealso [API documentation](https://cloud.google.com/bigquery/docs/reference/v2/projects/list)
#' @export
#' @examples
#' if (bq_authable()) {
#' bq_projects()
#' }
bq_projects <- function() {
  data <- bq_get("projects")$projects

  id <- unlist(lapply(data, function(x) x$id))
  names(id) <- vapply(data, function(x) x$friendlyName, character(1))

  id
}
