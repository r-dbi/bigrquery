#' Parse a BQ-style identifier into project/dataset IDs
#'
#' This function splits a dataset identifier (given as character) into its
#' components.
#'
#' @param dataset dataset name
#' @param project_id (optional) project ID to use if none is provided
#' in \code{dataset}
#' @return a list with \code{project_id} and \code{dataset_id} components
#' (either of which may be \code{NULL}).
#' @family identifier functions
#' @export
parse_dataset <- function(dataset, project_id = NULL) {
  assert_that(is.string(dataset),
              is.null(project_id) || is.string(project_id))
  first_split <- rsplit_one(dataset, ":")
  dataset_id <- first_split$right
  project_id <- first_split$left %||% project_id
  list(project_id = project_id, dataset_id = dataset_id)
}

#' Format dataset and project ID as a BQ-style identifier
#'
#' This function composes a dataset identifier from its individual
#' components.
#'
#' @param dataset dataset name
#' @param project_id project ID
#' @return a character.
#' @family identifier functions
#' @export
format_dataset <- function(project_id, dataset) {
  if (!is.null(project_id)) {
    dataset <- paste0(project_id, ":", dataset)
  }
  dataset
}

#' Parse a BQ-style identifier into project/dataset/table IDs
#'
#' This function splits a table identifier (given as character) into its
#' components.
#'
#' @param table table name
#' @param project_id (optional) project ID to use if none is provided
#' in \code{table}
#' @return a list with \code{project_id}, \code{dataset_id}, and
#' \code{table_id} components (any of which may be \code{NULL}).
#' @family identifier functions
#' @export
parse_table <- function(table, project_id = NULL) {
  assert_that(is.string(table), is.null(project_id) || is.string(project_id))
  dataset_id <- NULL
  first_split <- rsplit_one(table, ".")
  table_id <- first_split$right
  project_and_dataset <- first_split$left
  if (!is.null(project_and_dataset)) {
    second_split <- rsplit_one(project_and_dataset, ":")
    dataset_id <- second_split$right
    project_id <- second_split$left %||% project_id
  }
  list(project_id = project_id, dataset_id = dataset_id, table_id = table_id)
}

#' Format dataset, project and table ID as a BQ-style identifier
#'
#' This function composes a table identifier from its individual
#' components.
#'
#' @inheritParams format_dataset
#' @param table table ID
#' @return a character.
#' @family identifier functions
#' @export
format_table <- function(project_id, dataset, table) {
  if (!is.null(project_id)) {
    dataset <- paste0(project_id, ":", dataset)
  }
  table <- paste0(dataset, ".", table)
  table
}
