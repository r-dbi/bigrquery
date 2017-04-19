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

#' Gets an existing dataset in a project
#'
#' @param project The project name, a string
#' @param dataset The dataset to get, a string
#' @return a character vector of dataset names
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/get}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' get_dataset("publicdata", "shakespeare")
#' }
get_dataset <- function(project, dataset) {
  bq_get(bq_path(project, dataset))
}

#' @rdname get_dataset
#' @export
#' @description `exists_dataset` merely checks if a table exists, and returns
#'   either `TRUE` or `FALSE`.
exists_dataset <- function(project, dataset) {
  tryCatch(
    !is.null(get_dataset(project = project, dataset = dataset)),
    bigrquery_notFound = function(e) FALSE
  )
}

#' Deletes an existing dataset in a project
#'
#' @param project The project name, a string
#' @param dataset The dataset to delete, a string
#' @param deleteContents Whether to delete the tables if the dataset is not empty, a boolean
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/delete}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' delete_dataset("publicdata", "shakespeare", deleteContents = TRUE)
#' delete_dataset("myproject", "emptydataset")
#' }
delete_dataset <- function(project, dataset, deleteContents = FALSE) {
  assert_that(is.string(project), is.string(dataset))

  url <- sprintf("projects/%s/datasets/%s", project, dataset)
  bq_delete(url, query = list(deleteContents = deleteContents))
}

#' Creates a new dataset in a project
#'
#' @param project The project name, a string
#' @param dataset The name of the dataset to create, a string
#' @param description The dataset description, a string
#' @param friendlyName The dataset's friendly name, a string
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/insert}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' insert_dataset("myproject", "new_dataset")
#' }
insert_dataset <- function(project, dataset, description = NULL, friendlyName = NULL) {
  assert_that(is.string(project), is.string(dataset))

  url <- bq_path(project, "")

  body = list(
    datasetReference = list(
      projectId = project,
      datasetId = dataset
    ),
    description = description,
    friendlyName = friendlyName
  )

  bq_post(url, body)
}

#' Updates an existing dataset in a project
#'
#' @param project The project name, a string
#' @param dataset The dataset to update, a string
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
update_dataset <- function(project, dataset, description = NULL, friendlyName = NULL) {
  url <- bq_path(project, dataset)

  body = list(
    datasetReference = list(
      projectId = project,
      datasetId = dataset
    ),
    description = description,
    friendlyName = friendlyName
  )

  bq_put(url, body)
}
