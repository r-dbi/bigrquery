#' BigQuery table class
#'
#' `bq_table()` creates a reference from individual `project`, `dataset`, and
#' `table` identifiers; `as_bq_table()` will create from a character vector or
#' a list. See [table-API] for operations that work with the remote table.
#'
#' @export
#' @param project,dataset,table Path of identifier to a table.
#'   If `project` is a [bq_dataset], then `dataset` will be taken
#'   as the table name.
#' @param x An object to coerce to a `bq_table`.
#' @examples
#' natality <- bq_table("publicdata", "samples", "natality")
#' natality
#'
#' # If you already have a dataset
#' ds <- bq_dataset("publicdata", "samples")
#' bq_table(ds, "natality")
#'
#' as_bq_table("publicdata.samples.natality")
#'
#' # Included primarily for backward compatbility
#' as_bq_table(list(
#'   project_id = "publicdata",
#'   dataset_id = "samples",
#'   table_id = "natality"
#' ))
bq_table <- function(project, dataset, table = NULL) {
  if (inherits(project, "bq_dataset") && is.null(table)) {
    return(bq_table(project$project, project$dataset, dataset))
  }

  assert_that(is.string(project), is.string(dataset), is.string(table))

  structure(
    list(
      project = project,
      dataset = dataset,
      table = table
    ),
    class = "bq_table"
  )
}

#' @export
print.bq_table <- function(x, ...) {
  cat_line("<bq_table> ", x$project, ".", x$dataset, ".", x$table)
  invisible(x)
}

# Coercion ---------------------------------------------------------------

#' @export
#' @rdname bq_table
as_bq_table <- function(x) UseMethod("as_bq_table")

#' @export
as_bq_table.bq_table <- function(x) {
  x
}

#' @export
as_bq_table.character <- function(x) {
  assert_that(is.string(x))
  pieces <- strsplit(x, ".", fixed = TRUE)[[1]]

  if (length(pieces) != 3) {
    stop(
      "Character <bq_table> must have exactly three components when split by .",
      call. = FALSE)
  }
  bq_table(pieces[[1]], pieces[[2]], pieces[[3]])
}

#' @export
as_bq_table.list <- function(x) {
  if (!setequal(names(x), c("project_id", "dataset_id", "table_id"))) {
    stop(
      "List <bq_table> must have components 'project_id', 'dataset_id', and 'table_id'",
      call. = FALSE
    )
  }
  bq_table(x$project_id, x$dataset_id, x$table_id)
}

tableReference <- function(x) {
  x <- as_bq_table(x)

  list(
    projectId = unbox(x$project),
    datasetId = unbox(x$dataset),
    tableId = unbox(x$table)
  )
}
