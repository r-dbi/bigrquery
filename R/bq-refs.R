#' S3 classes that reference remote BigQuery datasets, tables and jobs
#'
#' @description
#' Create references to BigQuery datasets, jobs, and tables. Each class
#' has a constructor function (`bq_dataset()`, `bq_table()`, `bq_job()`)
#' and a coercion function (`as_bq_dataset()`, `as_bq_table()`, `as_bq_job()`).
#' The coercions functions come with methods for strings (which find components
#' by splitting on `.`), and lists (which look for named components like
#' `projectId` or `project_id`).
#'
#' All `bq_table_`, `bq_dataset_` and `bq_job_` functions call the appropriate
#' coercion functions on their first argument, allowing you to flexible specify
#' their inputs.
#'
#' @param project,dataset,table,job Individual project, dataset, table,
#'   and job identifiers (strings).
#'
#'   For `bq_table()`, you if supply a `bq_dataset` as the first argument,
#'   the 2nd argument will be interpreted as the `table`
#' @param x An object to coerce to a `bq_job`, `bq_dataset`, or `bq_table`.
#'   Built-in methods handle strings and lists.
#' @param ... Other arguments passed on to methods.
#' @seealso [api-job], [api-perform], [api-dataset], and [api-table] for
#'   functions that work with these objects.
#' @examples
#' # Creation ------------------------------------------------
#' samples <- bq_dataset("publicdata", "samples")
#' natality <- bq_table("publicdata", "samples", "natality")
#' natality
#'
#' # Or
#' bq_table(samples, "natality")
#'
#' bq_job("bigrquery-examples", "m0SgFu2ycbbge6jgcvzvflBJ_Wft")
#'
#' # Coercion ------------------------------------------------
#' as_bq_dataset("publicdata.shakespeare")
#' as_bq_table("publicdata.samples.natality")
#'
#' as_bq_table(list(
#'   project_id = "publicdata",
#'   dataset_id = "samples",
#'   table_id = "natality"
#' ))
#'
#' as_bq_job(list(
#'   projectId = "bigrquery-examples",
#'   jobId = "job_m0SgFu2ycbbge6jgcvzvflBJ_Wft",
#'   location = "US"
#' ))
#'
#' @name bq_refs
#' @aliases NULL
NULL

# dataset -----------------------------------------------------------------

#' @rdname bq_refs
#' @export
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

setOldClass("bq_dataset")

#' @export
print.bq_dataset <- function(x, ...) {
  cat_line("<bq_dataset> ", x$project, ".", x$dataset)
  invisible(x)
}

#' @rdname bq_refs
#' @export
as_bq_dataset <- function(x) UseMethod("as_bq_dataset")

#' @export
as_bq_dataset.bq_dataset <- function(x) x

#' @export
as_bq_dataset.character <- function(x) {
  x <- bq_from_string(x, 2, "bq_dataset")
  bq_dataset(x[[1]], x[[2]])
}

#' @export
as_bq_dataset.list <- function(x) {
  x <- bq_from_list(x, c("projectId", "datasetId"), "bq_dataset")
  bq_dataset(x$projectId, x$datasetId)
}

# table -------------------------------------------------------------------

#' @rdname bq_refs
#' @export
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

setOldClass("bq_table")

#' @export
print.bq_table <- function(x, ...) {
  cat_line("<bq_table> ", x$project, ".", x$dataset, ".", x$table)
  invisible(x)
}

#' @rdname bq_refs
#' @export
as_bq_table <- function(x, ...) UseMethod("as_bq_table")

#' @export
as_bq_table.bq_table <- function(x, ...) {
  x
}

#' @export
as_bq_table.character <- function(x, ...) {
  x <- bq_from_string(x, 3, "bq_table")
  bq_table(x[[1]], x[[2]], x[[3]])
}

#' @export
as_bq_table.list <- function(x, ...) {
  x <- bq_from_list(x, c("projectId", "datasetId", "tableId"), "bq_table")
  bq_table(x$projectId, x$datasetId, x$tableId)
}

# job ---------------------------------------------------------------------

#' @rdname bq_refs
#' @param location Job location
#' @export
bq_job <- function(project, job, location = "US") {
  structure(
    list(
      project = project,
      job = job,
      location = location
    ),
    class = "bq_job"
  )
}

#' @rdname bq_refs
#' @export
as_bq_job <- function(x) UseMethod("as_bq_job")

#' @export
as_bq_job.bq_job <- function(x) x

#' @export
as_bq_job.list <- function(x) {
  x <- bq_from_list(x, c("projectId", "jobId", "location"), "bq_job")
  bq_job(x$projectId, x$jobId, x$location)
}

#' @export
as_bq_job.character <- function(x) {
  x <- bq_from_string(x, 3, "bq_job")
  bq_job(x[[1]], x[[2]], x[[3]])
}

#' @export
print.bq_job <- function(x, ...) {
  cat_line("<bq_job> ", as.character(x))
  invisible(x)
}

#' @export
as.character.bq_job <- function(x, ...) {
  paste0(x$project, ".", x$job, ".", x$location)
}

# JSON --------------------------------------------------------------------

datasetReference <- function(x) {
  x <- as_bq_dataset(x)
  list(
    projectId = unbox(x$project),
    datasetId = unbox(x$dataset)
  )
}

tableReference <- function(x) {
  x <- as_bq_table(x)
  list(
    projectId = unbox(x$project),
    datasetId = unbox(x$dataset),
    tableId = unbox(x$table)
  )
}

# Helpers -----------------------------------------------------------------

bq_from_list <- function(x, names, type) {
  names(x) <- camelCase(names(x))

  if (length(setdiff(names, names(x))) == 0)
    return(x)

  names_str <- glue_collapse(names, sep = ", ", last = " and ")
  stop(glue("List <{type}> must have components {names_str}"), call. = FALSE)
}

bq_from_string <- function(x, n, type) {
  assert_that(is.string(x))

  pieces <- strsplit(x, ".", fixed = TRUE)[[1]]
  if (length(pieces) != n) {
    stop(
      glue("Character <{type}> must contain {n} components when split by `.`"),
      call. = FALSE
    )
  }
  pieces
}

