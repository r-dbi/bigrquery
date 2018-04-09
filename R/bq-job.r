#' BigQuery job class
#'
#' @param project,job Project and job identifiers
#' @param x An object to coerce to a job; usually a string.
#' @examples
#' \dontrun{
#' bq_job("bigrquery-examples", "m0SgFu2ycbbge6jgcvzvflBJ_Wft")
#' as_bq_job("bigrquery-examples:US.job_m0SgFu2ycbbge6jgcvzvflBJ_Wft")
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
as_bq_job.character <- function(x) {
  assert_that(length(x) == 1)

  pieces <- strsplit(x, ".", fixed = TRUE)[[1]]
  if (length(pieces) != 2) {
    stop(
      "Character <bq_job> must contain two components when split by `.`",
      call. = FALSE
    )
  }

  bq_job(pieces[[1]], pieces[[2]])
}

#' @export
print.bq_job <- function(x, ...) {
  cat_line("<bq_job> ", x$project, ".", x$job)
  invisible(x)
}

