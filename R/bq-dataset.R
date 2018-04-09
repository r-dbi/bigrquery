#' BigQuery dataset class
#'
#' `bq_dataset()` will create a reference to dataset from the `project` and
#' `dataset` identifiers; `as_bq_dataset()` will create a dataset from a string.
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
  cat_line("<bq_dataset> ", x$project, ".", x$dataset)
  invisible(x)
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
      "Character <bq_dataset> must contain two components when split by `.`",
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
