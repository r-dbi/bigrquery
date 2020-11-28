
#' Check status of a job [deprecated]
#'
#' Please use [api-job] instead
#'
#' @param project project name
#' @param job job id
#' @return a job resource list, as documented at
#'   \url{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs}
#' @seealso API documentation for get method:
#'   \url{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/get}
#' @seealso [wait_for()] to wait for a job to complete
#' @family jobs
#' @export
#' @keywords internal
get_job <- function(project, job) {

  .Deprecated("bq_job_meta", package = "bigrquery")

  assert_that(is.string(project), is.string(job))
  bq_get(bq_path(project, jobs = job))
}


#' Wait for a job to complete [deprecated]
#'
#' Please use [api-job] instead
#'
#' @param job job to wait for. Probably result of [insert_query_job()]
#'   or [insert_upload_job()]
#' @param quiet if `FALSE` print informative progress messages, if
#'   `TRUE` is silent, if `NA` displays messages for long-running
#'   jobs.
#' @param pause amount of time to wait between status requests
#' @keywords internal
#' @family jobs
#' @export
#' @keywords internal
wait_for <- function(job, quiet = getOption("bigrquery.quiet"), pause = 0.5) {

  .Deprecated("bq_job_wait", package = "bigrquery")

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
    signal_reason(err$reason, err$message)
  }

  progress$update(1)
  if (!isFALSE(quiet)) {
    if ("load" %in% names(job$configuration)) {
      in_bytes <- as.numeric(job$statistics$load$inputFileBytes)
      out_bytes <- as.numeric(job$statistics$load$outputBytes)
      message(format(size_units(in_bytes)), " input")
      message(format(size_units(out_bytes)), " output")
    } else if ("query" %in% names(job$configuration)) {
      bytes <- as.numeric(job$statistics$query$totalBytesBilled)
      message(format(size_units(bytes)), " processed")
    }
  }

  invisible(job)
}


size_units <- function(x) {
  i <- floor(log2(x) / 10)
  unit <- c("", "kilo", "mega", "giga", "tera", "peta", "exa", "zetta", "yotta")[i + 1]

  structure(x, i = i, unit = unit, class = "size")
}
#' @export
format.size <- function(x, ...) {
  if (x == 0) return("0 bytes")

  y <- x * 1024 ^ -attr(x, "i")
  sprintf("%.1f %sbytes", y, attr(x, "unit"))
}

