#' S3 classes for BigQuery datasets, tables and jobs
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
#' @param project,dataset,table,job,type Individual project, dataset, table,
#'   job identifiers and table type (strings).
#'
#'   For `bq_table()`, you if supply a `bq_dataset` as the first argument,
#'   the 2nd argument will be interpreted as the `table`
#' @param x An object to coerce to a `bq_job`, `bq_dataset`, or `bq_table`.
#'   Built-in methods handle strings and lists.
#' @inheritParams rlang::args_error_context
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
  check_string(project)
  check_string(dataset)

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
as_bq_dataset <- function(x,
                          ...,
                          error_arg = caller_arg(x),
                          error_call = caller_env()) {
  UseMethod("as_bq_dataset")
}

#' @export
as_bq_dataset.bq_dataset <- function(x,
                                     ...,
                                     error_arg = caller_arg(x),
                                     error_call = caller_env()) {
  x
}

#' @export
as_bq_dataset.character <- function(x,
                                    ...,
                                    error_arg = caller_arg(x),
                                    error_call = caller_env()) {
  x <- bq_from_string(x, n = 2, error_arg = error_arg, error_call = error_call)
  bq_dataset(x[[1]], x[[2]])
}

#' @export
as_bq_dataset.list <- function(x,
                               ...,
                               error_arg = caller_arg(x),
                               error_call = caller_env()) {
  x <- bq_from_list(
    x,
    names = c("projectId", "datasetId"),
    error_arg = error_arg,
    error_call = error_call
  )
  bq_dataset(x$projectId, x$datasetId)
}

#' @export
as_bq_dataset.default <- function(x,
                                  ...,
                                  error_arg = caller_arg(x),
                                  error_call = caller_env()) {
  cli::cli_abort(
    "{.arg {error_arg}} must be a string, list, or {.fun bq_dataset}.",
    call = error_call
  )
}


# table -------------------------------------------------------------------

#' @rdname bq_refs
#' @export
bq_table <- function(project, dataset, table = NULL, type = "TABLE") {
  if (inherits(project, "bq_dataset") && is.null(table)) {
    check_string(dataset)
    table <- dataset
    dataset <- project$dataset
    project <- project$project
  } else {
    check_string(project)
    check_string(dataset)
    check_string(table)
  }

  structure(
    list(
      project = project,
      dataset = dataset,
      table = table,
      type = type
    ),
    class = "bq_table"
  )
}

setOldClass("bq_table")

#' @export
print.bq_table <- function(x, ...) {
  cat_line("<bq_table> ", toString(x))
  invisible(x)
}

#' @export
toString.bq_table <- function(x, ...) {
  paste0(x$project, ".", x$dataset, ".", x$table)
}

#' @rdname bq_refs
#' @export
as_bq_table <- function(x,
                        ...,
                        error_arg = caller_arg(x),
                        error_call = caller_env()) {
  UseMethod("as_bq_table")
}

#' @export
as_bq_table.bq_table <- function(x,
                                 ...,
                                 error_arg = caller_arg(x),
                                 error_call = caller_env()) {
  x
}

#' @export
as_bq_table.character <- function(x,
                                  ...,
                                  error_arg = caller_arg(x),
                                  error_call = caller_env()) {
  x <- bq_from_string(x, n = 3, error_arg = error_arg, error_call = error_call)
  bq_table(x[[1]], x[[2]], x[[3]], ...)
}

#' @export
as_bq_table.list <- function(x, ..., error_arg = caller_arg(x), error_call = caller_env()) {
  x <- bq_from_list(
    x,
    names = c("projectId", "datasetId", "tableId"),
    error_arg = error_arg,
    error_call = error_call
  )
  bq_table(x$projectId, x$datasetId, x$tableId, ...)
}

#' @export
as_bq_table.default <- function(x,
                                  ...,
                                  error_arg = caller_arg(x),
                                  error_call = caller_env()) {
  cli::cli_abort(
    "{.arg {error_arg}} must be a string, list, or {.fun bq_table}.",
    call = error_call
  )
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
as_bq_job <- function(x,
                      ...,
                      error_arg = caller_arg(x),
                      error_call = caller_env()) {
  UseMethod("as_bq_job")
}

#' @export
as_bq_job.bq_job <- function(x,
                             ...,
                             error_arg = caller_arg(x),
                             error_call = caller_env()) {
  x
}

#' @export
as_bq_job.list <- function(x,
                           ...,
                           error_arg = caller_arg(x),
                           error_call = caller_env()) {
  x <- bq_from_list(
    x,
    c("projectId", "jobId", "location"),
    error_arg = error_arg,
    error_call = error_call
  )
  bq_job(x$projectId, x$jobId, x$location)
}

#' @export
as_bq_job.default <- function(x,
                              ...,
                              error_arg = caller_arg(x),
                              error_call = caller_env()) {
  cli::cli_abort(
    "{.arg {error_arg}} must be a string, list, or {.fun bq_job}.",
    call = error_call
  )
}

#' @export
as_bq_job.character <- function(x,
                                ...,
                                error_arg = caller_arg(x),
                                error_call = caller_env()) {
  x <- bq_from_string(x, 3, error_arg = error_arg, error_call = error_call)
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

bq_from_list <- function(x,
                         names,
                         error_arg = caller_arg(x),
                         error_call = caller_env()) {
  names(x) <- camelCase(names(x))

  if (length(setdiff(names, names(x))) == 0)
    return(x)

  cli::cli_abort(
    "When {.arg {error_arg}} is a list, it must have components {.and {.str {names}}}.",
    call = error_call
  )
}

bq_from_string <- function(x,
                           n,
                           error_arg = caller_arg(x),
                           error_call = caller_env()) {
  check_string(x, call = error_call)

  pieces <- strsplit(x, ".", fixed = TRUE)[[1]]
  if (length(pieces) != n) {
    cli::cli_abort(
      "When {.arg {error_arg}} is a string, it must contain {n} components separted by {.str .}.",
      call = error_call
    )
  }
  pieces
}
