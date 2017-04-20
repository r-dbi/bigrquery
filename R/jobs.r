#' Check status of a job.
#'
#' @param project project name
#' @param job job id
#' @return a job resource list, as documented at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs}
#' @seealso API documentation for get method:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs/get}
#' @seealso [wait_for()] to wait for a job to complete
#' @family jobs
#' @export
get_job <- function(project, job) {
  assert_that(is.string(project), is.string(job))

  bq_get(bq_path(project, jobs = job))
}


#' Wait for a job to complete, optionally printing updates
#'
#' @param job job to wait for. Probably result of [insert_query_job()]
#'   or [insert_upload_job()]
#' @param quiet if `FALSE` print informative progress messages, if
#'   `TRUE` is silent, if `NA` displays messages for long-running
#'   jobs.
#' @param pause amount of time to wait between status requests
#' @family jobs
#' @export
wait_for <- function(job, quiet = getOption("bigrquery.quiet"), pause = 0.5) {
  progress <- bq_progress(
    "Running job :spin: :elapsed:",
    total = 1e7,
    quiet = quiet
  )

  job <- get_job(job$jobReference$projectId, job$jobReference$jobId)

  while (job$status$state != "DONE") {
    Sys.sleep(pause)
    progress$tick()
    job <- get_job(job$jobReference$projectId, job$jobReference$jobId)
    progress$tick()
  }

  err <- job$status$errorResult
  if (!is.null(err)) {
    # error message is sufficient for now, could also pull more detailed reason
    stop(err$message, call. = FALSE)
  }

  if (!isFALSE(quiet)) {
    if ("load" %in% names(job$configuration)) {
      in_bytes <- as.numeric(job$statistics$load$inputFileBytes)
      out_bytes <- as.numeric(job$statistics$load$outputBytes)
      message(format(size_units(in_bytes)), " input bytes")
      message(format(size_units(out_bytes)), " output bytes")
    } else if ("query" %in% names(job$configuration)) {
      bytes <- as.numeric(job$statistics$query$totalBytesBilled)
      message(format(size_units(bytes)), " processed")
    }
  }
  progress$update(1)

  invisible(job)
}

