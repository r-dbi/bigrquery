#' Dataset API [deprecated]
#'
#' Please use [api-dataset] instead.
#'
#' @keywords internal
#' @name dataset-dep
NULL

#' @export
#' @rdname dataset-dep
get_dataset <- function(project, dataset) {

  .Deprecated("bq_dataset_meta", package = "bigrquery")

  bq_get(bq_path(project, dataset))
}

#' @export
#' @rdname dataset-dep
exists_dataset <- function(project, dataset) {

  .Deprecated("bq_dataset_exists", package = "bigrquery")

  tryCatch(
    !is.null(get_dataset(project = project, dataset = dataset)),
    bigrquery_notFound = function(e) FALSE
  )
}

#' @export
#' @rdname dataset-dep
delete_dataset <- function(project, dataset, deleteContents = FALSE) {

  .Deprecated("bq_dataset_delete", package = "bigrquery")

  assert_that(is.string(project), is.string(dataset))

  url <- sprintf("projects/%s/datasets/%s", project, dataset)
  bq_delete(url, query = list(deleteContents = deleteContents))
}

#' @export
#' @rdname dataset-dep
insert_dataset <- function(project, dataset, ...) {

  .Deprecated("bq_dataset_create", package = "bigrquery")

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

#' @export
#' @rdname dataset-dep
update_dataset <- function(project, dataset, ...) {

  .Deprecated("bq_dataset_update", package = "bigrquery")

  url <- bq_path(project, dataset)
  body <- list(
    datasetReference = list(
      projectId = project,
      datasetId = dataset
    )
  )

  bq_patch(url, body = bq_body(body, ...))
}

#' @export
#' @rdname dataset-dep
list_tables <- function(project, dataset, page_size = 50, max_pages = Inf) {

  .Deprecated("bq_dataset_tables", package = "bigrquery")

  data <- bq_get_paginated(
    bq_path(project, dataset, ""),
    page_size = page_size,
    max_pages = max_pages,
    warn = FALSE
  )

  tables <- unlist(lapply(data, function(x) x$tables), recursive = FALSE)
  vapply(tables, function(x) x$tableReference$tableId, character(1L))
}
