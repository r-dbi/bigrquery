#' Run a asynchronous query and retrieve results.
#'
#' This is a high-level function that inserts a query job
#' (with \code{\link{insert_query_job}}), repeatedly checks the status (with
#' \code{\link{get_job}}) until it is complete, then retrieves the results
#' (with \code{\link{list_tabledata}})
#'
#' @inheritParams insert_query_job
#' @inheritParams list_tabledata
#' @seealso Google documentation describing asynchronous queries:
#'  \url{https://developers.google.com/bigquery/docs/queries#asyncqueries}
#'
#'  Google documentation for handling large results:
#'  \url{https://developers.google.com/bigquery/querying-data#largequeryresults}
#' @export
#' @examples
#' \dontrun{
#' project <- "fantastic-voyage-389" # put your project ID here
#' sql <- "SELECT year, month, day, weight_pounds FROM [publicdata:samples.natality] LIMIT 5"
#' query_exec(sql, project = project)
#' # Put the results in a table you own (which uses project by default)
#' query_exec(sql, project = project, destination_table = "my_dataset.results")
#' # Use a default dataset for the query
#' sql <- "SELECT year, month, day, weight_pounds FROM natality LIMIT 5"
#' query_exec(sql, project = project, default_dataset = "publicdata:samples")
#' }
query_exec <- function(query, project, destination_table = NULL,
                       default_dataset = NULL, page_size = 1e4, max_pages = 10,
                       warn = TRUE) {

  dest <- run_query_job(query = query, project = project,
                        destination_table = destination_table,
                        default_dataset = default_dataset)

  list_tabledata(dest$projectId, dest$datasetId, dest$tableId,
    page_size = page_size, max_pages = max_pages, warn = warn)
}

# Submits a query job, waits for it, and returns information on the destination
# table for further consumption by the list_tabledata* functions
run_query_job <- function(query, project, destination_table, default_dataset) {
  assert_that(is.string(query), is.string(project))

  job <- insert_query_job(query, project, destination_table = destination_table,
                          default_dataset = default_dataset)
  job <- wait_for(job)

  job$configuration$query$destinationTable
}
