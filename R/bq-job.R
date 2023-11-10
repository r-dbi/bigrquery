#' BigQuery job: retrieve metadata
#'
#' To perform a job, see [api-perform]. These functions all retrieve metadata
#' (in various forms) about an existing job.
#'
#' @section Google BigQuery API documentation:
#' * [get](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/get)
#'
#' @examplesIf bq_testable()
#' jobs <- bq_project_jobs(bq_test_project())
#' jobs[[1]]
#'
#' # Show statistics about job
#' bq_job_show_statistics(jobs[[1]])
#'
#' # Wait for job to complete
#' bq_job_wait(jobs[[1]])
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
    cli::cli_inform("Input: {prettyunits::pretty_bytes(in_bytes)}")
    cli::cli_inform("Output: {prettyunits::pretty_bytes(out_bytes)}")
  }

  if ("query" %in% names(stats)) {
    bytes <- as.numeric(stats$query$totalBytesBilled)
    cli::cli_inform("Billed: {prettyunits::pretty_bytes(bytes)}")
  }

  invisible(x)
}

#' @param quiet If `FALSE`, displays progress bar; if `TRUE` is silent;
#'   if `NA` picks based on whether or not you're in an interactive context.
#' @param pause amount of time to wait between status requests
#' @export
#' @name api-job
#' @inheritParams rlang::args_error_context
bq_job_wait <- function(x,
                        quiet = getOption("bigrquery.quiet"),
                        pause = 0.5,
                        call = caller_env()) {
  x <- as_bq_job(x)
  quiet <- check_quiet(quiet)
  check_number_decimal(pause)

  if (!quiet) {
    cli::cli_progress_bar(
      format = "Running job {x} {cli::pb_spin} {cli::pb_elapsed}",
      total = NA,
      clear = FALSE
    )
  }

  repeat {
    if (!quiet) cli::cli_progress_update()
    # https://cloud.google.com/bigquery/docs/error-messages
    # Switch to req_retry() when we move to httr2
    status <- tryCatch(
      bq_job_status(x),
      bigrquery_http_503 = function(err) NULL
    )
    if (!quiet) cli::cli_progress_update()

    if (!is.null(status) && status$state == "DONE") break
    Sys.sleep(pause)
  }
  if (!quiet) cli::cli_progress_done()


  errors <- status$errors
  if (length(errors) > 0) {
    if (length(errors) > 1) {
      # First error says to look in errors[]
      errors <- errors[-1]
    }

    bullets <- map_chr(errors, function(x) paste0(x$message, " [", x$reason, "]"))
    bullets <- set_names(bullets, "x")
    cli::cli_abort(c("Job {x} failed", bullets), call = call)
  }

  if (!quiet) {
    cli::cli_inform("Job complete")
    bq_job_show_statistics(x)
  }

  invisible(x)
}
