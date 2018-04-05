#' Reference to a BigQuery dataset
#'
#' `bq_dataset()` will create a dataset from the `project` and `dataset`
#' identifiers; `as_bq_dataset()` will create a dataset from a string.
#' See [dataset-API] for the things you can do with a dataset.
#'
#' @param project,dataset Project and dataset identifiers.
#' @param x An object to coerce to a `bq_dataset`
#' @export
#' @examples
#' bq_dataset("publicdata", "shakespeare")
#' as_bq_dataset("publicdata.shakespeare")
bq_dataset <- function(project, dataset) {
  assert_that(is.string(project), is.string(dataset))

  structure(
    list(
      project = project,
      dataset = dataset
    ),
    class = "bq_dataset"
  )
}

#' @export
print.bq_dataset <- function(x, ...) {
  cat_line("<bq_dataset> ", x$project, ":", x$dataset)
}

#' @export
#' @rdname bq_dataset
as_bq_dataset <- function(x) UseMethod("as_bq_dataset")

#' @export
as_bq_dataset.bq_dataset <- function(x) x

#' @export
as_bq_dataset.character <- function(x) {
  assert_that(length(x) == 1)

  pieces <- strsplit(x, ".", fixed = TRUE)[[1]]
  if (length(pieces) != 2) {
    stop(
      "Character `bq_dataset` must contain two components when split by `.`",
      call. = FALSE
    )
  }

  bq_dataset(pieces[[1]], pieces[[2]])
}

datasetReference <- function(x) {
  x <- as_bq_dataset(x)
  list(
    projectId = unbox(x$project),
    datasetId = unbox(x$dataset)
  )
}

# API methods -------------------------------------------------------------

#' Manipulate BigQuery datasets
#'
#' @param x A [bq_dataset]
#' @param recursive If `TRUE`, will recursively delete all contents of the dataset.
#' @param ... Additional arguments merged into the body of the
#'   request. `snake_case` will automatically be converted into
#'   `camelCase` so you can use consistent argument names.
#' @param page_size Number of items per page
#' @param max_pages Maximum number of pages to retrieve
#'
#' @section API documentation:
#' * [get](https://cloud.google.com/bigquery/docs/reference/v2/datasets/get)
#' * [insert](https://cloud.google.com/bigquery/docs/reference/v2/datasets/insert)
#' * [update](https://cloud.google.com/bigquery/docs/reference/v2/datasets/insert)
#' * [delete](https://cloud.google.com/bigquery/docs/reference/v2/datasets/delete)
#' * [tables](https://cloud.google.com/bigquery/docs/reference/v2/tables/list)
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
#' @name dataset-API
NULL

#' @export
#' @rdname dataset-API
bq_dataset_meta <- function(x) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  bq_get(url)
}

#' @export
#' @rdname dataset-API
bq_dataset_exists <- function(x) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  bq_exists(url)
}

#' @export
#' @rdname dataset-API
bq_dataset_delete <- function(x, recursive = FALSE) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  query <- list(deleteContents = recursive)
  bq_delete(url, query = query)
}

#' @export
#' @rdname dataset-API
bq_dataset_create <- function(x, ...) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, "")
  body <- list(datasetReference = datasetReference(x))
  bq_post(url, body = bq_body(body, ...))

  x
}

#' @export
#' @rdname dataset-API
bq_dataset_udpate <- function(x, ...) {
  x <- as_bq_dataset(x)

  url <- bq_path(x$project, x$dataset)
  body <- list(datasetReference = datasetReference(x))
  bq_put(url, body = bq_body(body, ...))

  invisible(x)
}

#' @export
#' @rdname dataset-API
bq_dataset_tables <- function(x, page_size = 50, max_pages = Inf, ...) {
  x <- as_bq_dataset(x)
  url <- bq_path(x$project, x$dataset, "")

  data <- bq_get_paginated(url, page_size = page_size, max_pages = max_pages)

  tables <- unlist(lapply(data, function(x) x$tables), recursive = FALSE)

  lapply(tables, function(x) {
    ref <- x$tableReference
    bq_table(ref$projectId, ref$datasetId, ref$tableId)
  })
}
