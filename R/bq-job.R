#' This methods are all wrapper around a single API endpoint: `get`.
#'
#' @section API documentation:
#' * [meta](https://developers.google.com/bigquery/docs/reference/v2/jobs/get)
#'
#' @examples
#' if (bq_testable()) {
#' jobs <- bq_project_jobs(bq_test_project())
#' jobs[[1]]
#'
#' str(bq_job_meta(jobs[[1]]))
#'
#' # This will wait for a job to complete: if it's already complete
#' # it will display some status information
#' bq_job_wait(jobs[[1]])
#'
#' }
#' @name job-API
NULL

#' @export
#' @name job-API
#' @param x A [bq_job]
#' @param fields An optional field specification for
#'   [partial response](https://cloud.google.com/bigquery/docs/api-performance#partial-response)
bq_job_meta <- function(x, fields = NULL) {
  x <- as_bq_job(x)
  bq_get(
    bq_path(x$project, jobs = x$job),
    query = list(fields = fields)
  )
}

#' @export
#' @name job-API
bq_job_status <- function(x) {
  bq_job_meta(x, "status")$status
}

#' @export
#' @name job-API
bq_job_statistics <- function(x) {
  bq_job_meta(x, "statistics")$statistics
}

#' @param quiet if `FALSE` print informative progress messages, if
#'   `TRUE` is silent, if `NA` displays messages only for long-running
#'   jobs.
#' @param pause amount of time to wait between status requests
#' @export
#' @name job-API
bq_job_wait <- function(x, quiet = getOption("bigrquery.quiet"), pause = 0.5) {
  x <- as_bq_job(x)

  progress <- bq_progress(
    "Running job :spin: :elapsed:",
    total = 1e7,
    quiet = quiet
  )

  status <- bq_job_status(x)
  while (status$state != "DONE") {
    Sys.sleep(pause)
    progress$tick()
    status <- bq_job_status(x)
    progress$tick()
  }
  progress$update(1)

  err <- status$errorResult
  if (!is.null(err)) {
    signal_reason(err$reason, err$message)
  }

  if (!isFALSE(quiet)) {
    stats <- bq_job_statistics(x)

    message("Complete")
    if ("load" %in% names(stats)) {
      in_bytes <- as.numeric(stats$load$inputFileBytes)
      out_bytes <- as.numeric(stats$load$outputBytes)
      message("Input:  ", prettyunits::pretty_bytes(in_bytes))
      message("Output: ", prettyunits::pretty_bytes(out_bytes))
    }

    if ("query" %in% names(stats)) {
      bytes <- as.numeric(stats$query$totalBytesBilled)
      message("Billed: ", prettyunits::pretty_bytes(bytes))
    }
  }

  invisible(x)
}
