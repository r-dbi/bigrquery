#' BigQuery datasets
#'
#' Basic create-read-update-delete verbs for datasets.
#'
#' @param x A [bq_dataset]
#' @inheritParams api-job
#' @inheritParams api-perform
#' @inheritParams bq_projects
#'
#' @section API documentation:
#' * [get](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/get)
#' * [insert](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/insert)
#' * [delete](https://cloud.google.com/bigquery/docs/reference/rest/v2/datasets/delete)
#' * [list](https://cloud.google.com/bigquery/docs/reference/rest/v2/tables/list)
#' @examples
#' if (bq_testable()) {
#' ds <- bq_dataset(bq_test_project(), "dataset_api")
#' bq_dataset_exists(ds)
#'
#' bq_dataset_create(ds)
#' bq_dataset_exists(ds)
#' str(bq_dataset_meta(ds))
#'
#' bq_dataset_delete(ds)
#' bq_dataset_exists(ds)
#'
#' # Use bq_test_dataset() to create a temporary dataset that will
#' # be automatically deleted
#' ds <- bq_test_dataset()
#' bq_table_create(bq_table(ds, "x1"))
#' bq_table_create(bq_table(ds, "x2"))
#' bq_table_create(bq_table(ds, "x3"))
#' bq_dataset_tables(ds)
#' }
#' @name api-dataset
NULL

#' @export
#' @rdname api-dataset
#' @param location Dataset location
bq_dataset_create <- function(x, location = "US", ...) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, "")
  body <- list(
    datasetReference = datasetReference(x),
    location = location
  )
  bq_post(url, body = bq_body(body, ...))

  x
}

#' @export
#' @rdname api-dataset
bq_dataset_meta <- function(x, fields = NULL) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  bq_get(url, query = list(fields = fields))
}

#' @export
#' @rdname api-dataset
bq_dataset_exists <- function(x) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  bq_exists(url)
}

#' @export
#' @rdname api-dataset
bq_dataset_update <- function(x, ...) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  body <- list(datasetReference = datasetReference(x))
  bq_patch(url, body = bq_body(body, ...))

  invisible(x)
}

#' @export
#' @rdname api-dataset
#' @param delete_contents If `TRUE`, will recursively delete all tables in
#'   the dataset. Set to `FALSE` by default for safety.
bq_dataset_delete <- function(x, delete_contents = FALSE) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  query <- list(deleteContents = delete_contents)
  bq_delete(url, query = query)

  invisible(x)
}

#' @export
#' @rdname api-dataset
bq_dataset_tables <- function(x, page_size = 50, max_pages = Inf, warn = TRUE, ...) {
  x <- as_bq_dataset(x)
  url <- bq_path(x$project, x$dataset, "")

  data <- bq_get_paginated(
    url,
    query = list(fields = "tables(tableReference)"),
    page_size = page_size,
    max_pages = max_pages,
    warn = warn
  )

  tables <- unlist(lapply(data, function(x) x$tables), recursive = FALSE)
  lapply(tables, function(x) as_bq_table(x$tableReference))
}
