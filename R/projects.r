# https://developers.google.com/bigquery/docs/reference/v2/projects

#' List projects that you have access to
#'
#' @return a character vector of project ids named with their friendly names
#'   Always includes publicly available sampledata project.
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
  names(id) <- unlist(lapply(data, function(x) x$friendlyName))
  c(id, "sampledata" = "sampledata")
}
