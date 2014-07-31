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
#'  
#'  Google documentation for handling large results:
#'  \url{https://developers.google.com/bigquery/querying-data#largequeryresults}
#' @export
#' @examples
#' \donttest{
#' billing_project <- "341409650721" # put your project number here
#' sql <- "SELECT year, month, day, weight_pounds FROM natality LIMIT 5"
#' query_exec("publicdata", "samples", sql, billing = billing_project)
#' }
query_exec <- function(project, dataset, query, billing = project,
                       page_size = 1e4, max_pages = 10, warn = TRUE,
                       destination = NULL) {
  assert_that(is.string(project), is.string(dataset), is.string(query),
    is.string(billing))

  job <- insert_query_job(project, dataset, query, billing, destination)
  job <- wait_for(job)

  dest <- job$configuration$query$destinationTable
  list_tabledata(dest$projectId, dest$datasetId, dest$tableId,
    page_size = page_size, max_pages = max_pages, warn = warn)
}
