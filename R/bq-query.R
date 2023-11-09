#' Submit query to BigQuery
#'
#' These submit a query (using [bq_perform_query()]) and then wait for it
#' complete (with [bq_job_wait()]). All BigQuery queries save their results
#' into a table (temporary or otherwise), so these functions return a [bq_table]
#' which you can then query for more information.
#'
#' @param x Either a project (a string) or a [bq_dataset].
#' @param billing If you query a dataset that you only have read access
#'   for, such as a public dataset, you must also submit a `billing` project.
#' @inheritParams bq_perform_query
#' @inheritParams api-job
#' @param ... Passed on to [bq_perform_query()]
#' @name bq_query
#' @return A [bq_table]
#' @examplesIf bq_testable()
#' # Querying a project requires full name in query
#' tb <- bq_project_query(
#'   bq_test_project(),
#'   "SELECT count(*) FROM publicdata.samples.natality"
#' )
#' bq_table_fields(tb)
#' bq_table_download(tb)
#'
#' # Querying a dataset sets default dataset so you can use bare table name,
#' # but for public data, you'll need to set a project to bill.
#' ds <- bq_dataset("publicdata", "samples")
#' tb <- bq_dataset_query(ds,
#'   query = "SELECT count(*) FROM natality",
#'   billing = bq_test_project()
#' )
#' bq_table_download(tb)
#'
#' tb <- bq_dataset_query(ds,
#'   query = "SELECT count(*) FROM natality WHERE state = @state",
#'   parameters = list(state = "KS"),
#'   billing = bq_test_project()
#' )
#' bq_table_download(tb)
NULL

#' @export
#' @rdname bq_query
bq_project_query <- function(x,
                             query,
                             destination_table = NULL,
                             ...,
                             quiet = NA) {

  check_string(x)
  query <- as_query(query)
  if (!is.null(destination_table)) {
    destination_table <- as_bq_table(destination_table)
  }
  check_bool(quiet, allow_na = TRUE)

  job <- bq_perform_query(
    query,
    billing = x,
    destination_table = destination_table,
    ...
  )
  bq_job_wait(job, quiet = quiet)
  bq_job_table(job)
}

#' @export
#' @rdname bq_query
bq_dataset_query <- function(x,
                             query,
                             destination_table = NULL,
                             ...,
                             billing = NULL,
                             quiet = NA) {


  x <- as_bq_dataset(x)
  query <- as_query(query)
  if (!is.null(destination_table)) {
    destination_table <- as_bq_table(destination_table)
  }
  check_string(billing, allow_null = TRUE)
  check_bool(quiet, allow_na = TRUE)

  job <- bq_perform_query(
    query,
    billing = billing %||% x$project,
    destination_table = destination_table,
    default_dataset = x,
    ...
  )
  bq_job_wait(job, quiet = quiet)
  bq_job_table(job)
}
