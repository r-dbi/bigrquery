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
#' \donttest{
#' library(bigrquery)
#' billing_project <- "341409650721" # put your project number here
#' sql <- "SELECT year, month, day, weight_pounds FROM natality LIMIT 5"
#' query_exec("publicdata", "samples", sql, billing = billing_project)
#' }
query_exec <- function(project, dataset, query, billing = project,
                       page_size = 1e4, max_pages = 10) {
  assert_that(is.string(project), is.string(dataset), is.string(query),
    is.string(billing))

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

  bytes <- as.numeric(job$statistics$totalBytesProcessed)
  message(format(size_units(bytes)), " processed")

  dest <- job$configuration$query$destinationTable
  list_tabledata(dest$projectId, dest$datasetId, dest$tableId)
}


size_units <- function(x) {
  i <- floor(log2(x) / 10)
  unit <- c("", "kilo", "mega", "giga", "tera", "peta", "exa", "zetta", "yotta")[i + 1]

  structure(x, i = i, unit = unit, class = "size")
}
#' @S3method format size
format.size <- function(x, ...) {
  y <- x * 1024 ^ -attr(x, "i")
  sprintf("%.1f %sbytes", y, attr(x, "unit"))
}
