#' Create a table reference
#'
#' `bq_table()` creates from the individual components; `as_bq_table()`
#' will create from a character vector or a list.
#'
#' @export
#' @examples
#' natality <- bq_table("publicdata", "samples", "natality")
#' natality
#' get_table(natality)
#'
#' as_bq_table("publicdata:samples:natality")
#' as_bq_table("publicdata.samples.natality")
#'
#' # Included primarily for backward compatbility
#' as_bq_table(list(
#'   project_id = "publicdata",
#'   dataset_id = "sameples",
#'   table_id = "natality"
#' ))
bq_table <- function(project, dataset, table) {
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
  pieces <- strsplit(x, "[:.]")[[1]]

  if (length(pieces) != 3) {
    stop(
      "Character <bq_table> must have exactly three components when split by ./:",
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

# Helpers -----------------------------------------------------------------

tableReference <- function(x) {
  list(
    projectId = unbox(x$project),
    datasetId = unbox(x$dataset),
    tableId = unbox(x$table)
  )
}
