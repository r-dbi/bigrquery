# https://developers.google.com/bigquery/docs/reference/v2/projects

#' List all projects to which you have been granted any project role.
#'
#' You can also work with [public datasets](https://cloud.google.com/bigquery/public-data/)
#' but you will need to provide a `billing` project whenever you perform
#' any non-free operation.
#'
#' @return A character vector of project ids named with their friendly names.
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
