#' List the datasets in a project
#'
#' @param project The project name, a string
#' @return a character vector of dataset names
#' @seealso Google API documentation:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/datasets/list}
#' @family datasets
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

#' Gets an existing dataset in a project
#'
#' @param project The project name, a string
#' @param datasetId The datasetId to get, a string
#' @return a character vector of dataset names
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/get}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' get_dataset("publicdata", "shakespeare")
#' }
get_dataset <- function(project, datasetId) {
  assert_that(is.string(project), is.string(datasetId))

  url <- sprintf("projects/%s/datasets/%s", project, datasetId)
  bq_get(url)
}

#' Deletes an existing dataset in a project
#'
#' @param project The project name, a string
#' @param datasetId The datasetId to delete, a string
#' @param deleteContents Whether to delete the tables if the dataset is not empty, a boolean
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/delete}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' delete_dataset("publicdata", "shakespeare", TRUE)
#' delete_dataset("myproject", "emptydataset")
#' }
delete_dataset <- function(project, datasetId, deleteContents = FALSE) {
  assert_that(is.string(project), is.string(datasetId))

  url <- sprintf("projects/%s/datasets/%s", project, datasetId)
  bq_delete(url, query = list(deleteContents = deleteContents))
}

#' Creates a new dataset in a project
#'
#' @param project The project name, a string
#' @param datasetId The datasetId to delete, a string
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/delete}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' insert_dataset("myproject", "new_dataset")
#' }
insert_dataset <- function(project, datasetId) {
  assert_that(is.string(project), is.string(datasetId))

  url <- sprintf("projects/%s/datasets", project)

  body = list(
    datasetReference = list(
      projectId = project,
      datasetId = datasetId
    )
  )

  bq_post(url, body)
}

#' Updates an existing dataset in a project
#'
#' @param project The project name, a string
#' @param datasetId The datasetId to delete, a string
#' @param description The dataset description, a string
#' @param friendlyName The dataset's friendly name, a string
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/update}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' update_dataset("myproject", "existing_dataset", "my description", "friendly name")
#' }
update_dataset <- function(project, datasetId, description = NULL, friendlyName = NULL) {
  assert_that(is.string(project), is.string(datasetId))

  url <- sprintf("projects/%s/datasets/%s", project, datasetId)

  body = list(
    datasetReference = list(
      projectId = project,
      datasetId = datasetId
    ),
    description = description,
    friendlyName = friendlyName
  )

  bq_put(url, body)
}
