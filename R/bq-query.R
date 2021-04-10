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
#' @examples
#' if (bq_testable()) {
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
#' }
NULL

#' @export
#' @rdname bq_query
bq_project_query <- function(x, query,
                             destination_table = NULL,
                             ...,
                             quiet = NA) {
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
bq_dataset_query <- function(x, query,
                             destination_table = NULL,
                             ...,
                             billing = NULL,
                             quiet = NA) {
  x <- as_bq_dataset(x)
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

# Similar to bq_perform_query: except uses dryRun, and hence result
# processing is totally different
bq_query_fields <- function(query, x, ..., default_dataset = NULL, use_legacy_sql = FALSE) {
  assert_that(is.string(x), is.string(query))

  url <- bq_path(x, jobs = "")

  query <- list(
    query = unbox(query),
    useLegacySql = unbox(use_legacy_sql)
  )
  if (!is.null(default_dataset)) {
    query$defaultDataset <- datasetReference(default_dataset)
  }

  body <- list(configuration = list(query = query, dryRun = TRUE))

  res <- bq_post(
    url,
    body = bq_body(body, ...),
    query = list(fields = "statistics(query(schema(fields)))")
  )

  fields <- res$statistics$query$schema$fields
  bq_fields(lapply(fields, as_bq_field))
}
