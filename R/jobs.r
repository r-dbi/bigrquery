#' Create a new query job.
#'
#' This is a low-level function that creates a query job. To wait until it
#' is finished and then retrieve the results, see \code{\link{query_exec}}
#'
#' @param project project name
#' @param dataset dataset name
#' @param query SQL query string
#' @param billing project to bill to, if different to \code{project}
#' @family jobs
#' @return a job resource list, as documented at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs}
#' @seealso API documentation for insert method:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs/insert}
#' @export
insert_query_job <- function(project, dataset, query, billing = project) {
  assert_that(is.string(project), is.string(dataset), is.string(query),
    is.string(billing))

  url <- sprintf("projects/%s/jobs", billing)
  body <- list(
    kind = "bigquery#job",
    configuration = list(
      query = list(
        query = query,
        defaultDataset = list(
          projectId = project,
          datasetId = dataset
        )
      )
    )
  )
  bq_post(url, body)
}

#' Check status of a job.
#'
#' @param project project name
#' @param job job id
#' @return a job resource list, as documented at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs}
#' @seealso API documentation for get method:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs/get}
#' @seealso \code{\link{wait_for}} to wait for a job to complete
#' @export
get_job <- function(project, job) {
  assert_that(is.string(project), is.string(job))

  url <- sprintf("projects/%s/jobs/%s", project, job)
  bq_get(url)
}


#' Wait for a job to complete, optionally printing updates
#' 
#' @param job job to wait for. Probably result of \code{\link{insert_query_job}}
#'   or \code{\link{insert_update_job}}
#' @param quiet if \code{FALSE} print informative progress messages
#' @param pause amount of time to wait between status requests
#' @export
wait_for <- function(job, quiet = FALSE, pause = 0.25) {
  elapsed <- timer()
  while(job$status$state != "DONE") {
    if (!quiet) {
      cat("\rRunning query:   ", job$status$state, " ",
        sprintf("%4.1f", elapsed()), "s", sep = "")
    }
    Sys.sleep(pause)
    job <- get_job(job$jobReference$projectId, job$jobReference$jobId)
  }
  if (!quiet) cat("\n")
  
  err <- job$status$errorResult
  if (!is.null(err)) {
    err_message <- function(x) paste0(x$location, " ", x$reason, ". ", x$message)
    errors <- vapply(job$status$errors, err_message, character(1))
    
    stop(err$message, "\n\n", paste0(errors, collapse = "\n"), call. = FALSE)
  }
  
  if (!quiet) {
    bytes <- as.numeric(job$statistics$totalBytesProcessed)
    message(format(size_units(bytes)), " processed")
  }
  
  job
}

size_units <- function(x) {
  i <- floor(log2(x) / 10)
  unit <- c("", "kilo", "mega", "giga", "tera", "peta", "exa", "zetta", "yotta")[i + 1]
  
  structure(x, i = i, unit = unit, class = "size")
}
#' @S3method format size
format.size <- function(x, ...) {
  if (x == 0) return("0 bytes")
  
  y <- x * 1024 ^ -attr(x, "i")
  sprintf("%.1f %sbytes", y, attr(x, "unit"))
}
