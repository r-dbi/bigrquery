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
#' @param ... Additional arguments merged into the body of the
#'   request. `snake_case` will automatically be converted into
#'   `camelCase` so you can use consistent argument names.
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/insert}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' insert_dataset("myproject", "new_dataset")
#' }
insert_dataset <- function(project, dataset, ...) {
  assert_that(is.string(project), is.string(dataset))

  url <- bq_path(project, "")
  body <- list(
    datasetReference = list(
      projectId = project,
      datasetId = dataset
    )
  )

  bq_post(url, body = bq_body(body, ...))
}

#' Updates an existing dataset in a project
#'
#' @inheritParams insert_dataset
#' @seealso Google API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/datasets/update}
#' @family datasets
#' @export
#' @examples
#' \dontrun{
#' update_dataset("myproject", "existing_dataset", "my description", "friendly name")
#' }
update_dataset <- function(project, dataset, ...) {
  url <- bq_path(project, dataset)
  body <- list(
    datasetReference = list(
      projectId = project,
      datasetId = dataset
    )
  )

  bq_put(url, body = bq_body(body, ...))
}

#' List available tables in dataset.
#'
#' @inheritParams get_table
#' @param page_size Number of items per page
#' @param max_pages Maximum number of pages to retrieve
#' @return a character vector of table names
#' @family tables
#' @seealso API documentation:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/tables/list}
#' @export
#' @examples
#' \dontrun{
#' list_tables("publicdata", "samples")
#' list_tables("githubarchive", "github")
#' list_tables("publicdata", "samples", max_pages = 2, page_size = 2)
#' }
list_tables <- function(project, dataset, page_size = 50, max_pages = Inf) {
  data <- bq_get_paginated(
    bq_path(project, dataset, ""),
    page_size = page_size,
    max_pages = max_pages
  )

  tables <- unlist(lapply(data, function(x) x$tables), recursive = FALSE)
  vapply(tables, function(x) x$tableReference$tableId, character(1L))
}
