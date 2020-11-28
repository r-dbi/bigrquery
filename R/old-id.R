#' Table/dataset objects [deprecated]
#'
#' Please use [bq_table] and [bq_dataset] instead.
#'
#' @keywords internal
#' @name id-dep
NULL

#' @export
#' @rdname id-dep
parse_dataset <- function(dataset, project_id = NULL) {

  .Deprecated("bq_dataset", package = "bigrquery")

  assert_that(is.string(dataset), is.null(project_id) || is.string(project_id))
  first_split <- rsplit_one(dataset, ":")
  dataset_id <- first_split$right
  project_id <- first_split$left %||% project_id
  list(project_id = project_id, dataset_id = dataset_id)
}

#' @export
#' @rdname id-dep
format_dataset <- function(project_id, dataset) {

  .Deprecated("bq_dataset", package = "bigrquery")

  if (!is.null(project_id)) {
    dataset <- paste0(project_id, ":", dataset)
  }
  dataset
}

#' @export
#' @rdname id-dep
parse_table <- function(table, project_id = NULL) {

  .Deprecated("bq_table", package = "bigrquery")

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

#' @export
#' @rdname id-dep
format_table <- function(project_id, dataset, table) {
  if (!is.null(project_id)) {
    dataset <- paste0(project_id, ":", dataset)
  }
  table <- paste0(dataset, ".", table)
  table
}

# Given a string and a separator, split the string at the rightmost
# occurrence of the separator and return the two parts.
rsplit_one <- function(str, sep) {
  assert_that(is.string(str), is.string(sep))
  parts <- strsplit(str, sep, fixed = TRUE)[[1]]
  right <- parts[length(parts)]
  if (length(parts) > 1) {
    left <- paste0(parts[-length(parts)], collapse = sep)
  } else {
    left <- NULL
  }
  list(left = left, right = right)
}
