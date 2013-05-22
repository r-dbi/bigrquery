# https://code.google.com/apis/console

#' Run a asynchronous query and retrieve results.
#'
#' This is a high-level function that inserts a query job
#' (with \code{\link{insert_query_job}}), repeatedly checks the status (with
#' \code{\link{get_job}}) until it is complete, then retrieves the results
#' (with \code{\link{list_tabledata}})
#'
#' @inheritParams insert_query_job
#' @seealso Google documentation describing asynchronous queries:
#'  \url{https://developers.google.com/bigquery/docs/queries#asyncqueries}
#' @export
#' @examples
#' \dontrun{
#' sql <- "SELECT year, avg(weight_pounds) FROM natality WHERE year > 2005
#'   GROUP BY year"
#' query_exec("publicdata", "samples", sql, billing = "yourproject")
#' }
query_exec <- function(project, dataset, query, billing = project) {
  assert_that(is.string(project), is.string(dataset), is.string(query),
    is.string(billing))

  cat("Running query")
  job <- insert_query_job(project, dataset, query, billing)
  jobref <- job$jobReference

  elapsed <- timer()
  while(job$status$state != "DONE") {
    cat("\rRunning query:   ", job$status$state, " ",
      sprintf("%4.1f", elapsed()), "s", sep = "")
    Sys.sleep(0.25)
    job <- get_job(jobref$projectId, jobref$jobId)
  }
  cat("\n")

  err <- job$status$errorResult
  if (!is.null(err)) {
    stop(err$message, call. = FALSE)
  }

  dest <- job$configuration$query$destinationTable
  list_tabledata(dest$projectId, dest$datasetId, dest$tableId)
}
