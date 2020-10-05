#' BigQuery job: retrieve metadata
#'
#' To perform a job, see [api-perform]. These functions all retrieve metadata
#' (in various forms) about an existing job.
#'
#' @section API documentation:
#' * [get](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/get)
#'
#' @examples
#' if (bq_testable()) {
#' jobs <- bq_project_jobs(bq_test_project())
#' jobs[[1]]
#'
#' # Show statistics about job
#' bq_job_show_statistics(jobs[[1]])
#'
#' # Wait for job to complete
#' bq_job_wait(jobs[[1]])
#' }
#' @name api-job
NULL

#' @export
#' @name api-job
#' @param x A [bq_job]
#' @param fields An optional field specification for
#'   [partial response](https://cloud.google.com/bigquery/docs/api-performance#partial-response)
bq_job_meta <- function(x, fields = NULL) {
  x <- as_bq_job(x)
  bq_get(
    bq_path(x$project, jobs = x$job),
    query = list(
      location = x$location,
      fields = fields
    )
  )
}

bq_job_table <- function(x) {
  meta <- bq_job_meta(x, "configuration(query(destinationTable))")
  as_bq_table(meta$configuration$query$destinationTable)
}

#' @export
#' @name api-job
bq_job_status <- function(x) {
  bq_job_meta(x, "status")$status
}

#' @export
#' @name api-job
bq_job_show_statistics <- function(x) {
  stats <- bq_job_meta(x, "statistics")$statistics

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

  invisible(x)
}

#' @param quiet If `FALSE`, displays progress bar; if `TRUE` is silent;
#'   if `NA` displays progress bar only for long-running jobs.
#' @param pause amount of time to wait between status requests
#' @export
#' @name api-job
bq_job_wait <- function(x, quiet = getOption("bigrquery.quiet"), pause = 0.5) {
  x <- as_bq_job(x)

  progress <- bq_progress(
    paste0("Running job '", x, "' [:spin] :elapsed"),
    total = 1e7,
    quiet = quiet,
    clear = FALSE
  )

  status <- bq_job_status(x)
  while (status$state != "DONE") {
    Sys.sleep(pause)
    progress$tick()
    status <- bq_job_status(x)
    progress$tick()
  }
  progress$update(1)

  errors <- status$errors
  if (length(errors) > 0) {
    if (length(errors) > 1) {
      # First error says to look in errors[]
      errors <- errors[-1]
    }

    bullets <- vapply(errors,
      function(x) paste0(x$message, " [", x$reason, "]"),
      character(1)
    )
    names(bullets) <- rep("x", length(bullets))

    rlang::abort(c(paste0("Job '", if (!isTRUE(quiet)) x, "' failed"), bullets))
  }

  if (isFALSE(quiet) || (is.na(quiet) && interactive())) {
    message("Complete")
    bq_job_show_statistics(x)
  }

  invisible(x)
}
