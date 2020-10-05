#' Run a asynchronous query and retrieve results [deprecated]
#'
#' Please use [bq_project_query()] instead.
#'
#' @inheritParams insert_query_job
#' @inheritParams bq_dataset_tables
#' @seealso Google documentation describing asynchronous queries:
#'  \url{https://cloud.google.com/bigquery/docs/running-queries}
#'
#'  Google documentation for handling large results:
#'  \url{https://cloud.google.com/bigquery/docs/writing-results}
#' @export
#' @examples
#' \dontrun{
#' project <- bq_test_project() # put your project ID here
#' sql <- "SELECT year, month, day, weight_pounds FROM [publicdata:samples.natality] LIMIT 5"
#' query_exec(sql, project = project)
#' # Put the results in a table you own (which uses project by default)
#' query_exec(sql, project = project, destination_table = "my_dataset.results")
#' # Use a default dataset for the query
#' sql <- "SELECT year, month, day, weight_pounds FROM natality LIMIT 5"
#' query_exec(sql, project = project, default_dataset = "publicdata:samples")
#' }
#' @keywords internal
query_exec <- function(query, project,
                       destination_table = NULL,
                       default_dataset = NULL,
                       page_size = 1e4,
                       max_pages = 10,
                       warn = TRUE,
                       create_disposition = "CREATE_IF_NEEDED",
                       write_disposition = "WRITE_EMPTY",
                       use_legacy_sql = TRUE,
                       quiet = getOption("bigrquery.quiet"),
                       ...) {

  .Deprecated("bq_perform_query", package = "bigrquery")

  dest <- run_query_job(
    query = query,
    project = project,
    destination_table = destination_table,
    default_dataset = default_dataset,
    create_disposition = create_disposition,
    write_disposition = write_disposition,
    use_legacy_sql = use_legacy_sql,
    quiet = quiet,
    ...
  )

  list_tabledata(
    dest$projectId,
    dest$datasetId,
    dest$tableId,
    page_size = page_size,
    max_pages = max_pages,
    warn = warn,
    quiet = quiet
  )
}

# Submits a query job, waits for it, and returns information on the destination
# table for further consumption by the list_tabledata* functions
run_query_job <- function(query,
                          project,
                          destination_table,
                          default_dataset,
                          create_disposition = "CREATE_IF_NEEDED",
                          write_disposition = "WRITE_EMPTY",
                          use_legacy_sql = TRUE,
                          quiet = getOption("bigrquery.quiet"),
                          ...) {

  .Deprecated("bq_perform_query", package = "bigrquery")

  assert_that(is.string(query), is.string(project))

  job <- insert_query_job(
    query,
    project,
    destination_table = destination_table,
    default_dataset = default_dataset,
    create_disposition = create_disposition,
    write_disposition = write_disposition,
    use_legacy_sql = use_legacy_sql,
    ...
  )
  job <- wait_for(job, quiet = quiet)

  job$configuration$query$destinationTable
}
