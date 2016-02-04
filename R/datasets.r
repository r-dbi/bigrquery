#' List the datasets in a project
#'
#' @param project The project name, a string
#' @return a character vector of dataset names
#' @seealso Google API documentation:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/datasets/list}
#' @export
#' @examples
#' \dontrun{
#' list_datasets("publicdata")
#' list_datasets("githubarchive")
#' }
list_datasets <- function(project) {
  assert_that(is.string(project))

  url <- sprintf("projects/%s/datasets", project)
  data <- bq_get(url)$datasets

  unlist(lapply(data, function(x) x$datasetReference$datasetId))
}
