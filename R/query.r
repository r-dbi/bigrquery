# https://code.google.com/apis/console

#' @examples
#' sql <- "SELECT year, avg(weight_pounds) FROM natality WHERE year > 2005 GROUP BY year"
#' query_exec("publicdata", "samples", sql, billing = "605948885573")
query_exec <- function(project, dataset, query, billing = project) {
  cat("Running query")
  job <- insert_job(project, dataset, query, billing)
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
