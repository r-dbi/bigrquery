#' BigQuery job class
#'
#' Create a job from `project` and `job` identifiers with `bq_job()`,
#' or from BQ's standard string representation with `as_bq_job()`.
#'
#' @param project,job Project and job identifiers
#' @param x An object to coerce to a job; usually a string.
#' @examples
#' \dontrun{
#' bq_job("bigrquery-examples", "m0SgFu2ycbbge6jgcvzvflBJ_Wft")
#'
#' as_bq_job("bigrquery-examples:US.job_m0SgFu2ycbbge6jgcvzvflBJ_Wft")
#' as_bq_job(list(
#'   projectId = "bigrquery-examples",
#'   jobId = "US.job_m0SgFu2ycbbge6jgcvzvflBJ_Wft"
#' ))
#' }
bq_job <- function(project, job) {
  structure(
    list(
      project = project,
      job = job
    ),
    class = "bq_job"
  )
}

#' @export
#' @rdname bq_job
as_bq_job <- function(x) UseMethod("as_bq_job")

#' @export
as_bq_job.bq_job <- function(x) x

#' @export
as_bq_job.list <- function(x) {
  x <- bq_from_list(x, c("projectId", "jobId"), "bq_job")
  bq_job(x$projectId, x$jobId)
}

#' @export
as_bq_job.character <- function(x) {
  x <- bq_from_string(x, 2, "bq_job")
  bq_job(x[[1]], x[[2]])
}

#' @export
print.bq_job <- function(x, ...) {
  cat_line("<bq_job> ", x$project, ".", x$job)
  invisible(x)
}

