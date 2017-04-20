# https://developers.google.com/bigquery/docs/reference/v2/projects

#' List all projects to which you have been granted any project role.
#'
#' @return a character vector of project ids named with their friendly names.
#' @seealso API documentation at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/projects/list}
#' @export
#' @examples
#' \dontrun{
#' list_projects()
#' }
list_projects <- function() {
  data <- bq_get("projects")$projects

  id <- unlist(lapply(data, function(x) x$id))
  names(id) <- vapply(data, function(x) x$friendlyName, character(1))

  id
}
