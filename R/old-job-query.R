#' Create a new query job [deprecated]
#'
#' Please use [api-job] and [api-perform] instead.
#'
#' @keywords internal
#' @inheritParams insert_dataset
#' @param query SQL query string
#' @param destination_table (optional) destination table for large queries,
#'   either as a string in the format used by BigQuery, or as a list with
#'   `project_id`, `dataset_id`, and `table_id` entries
#' @param create_disposition behavior for table creation.
#'   defaults to `"CREATE_IF_NEEDED"`,
#'   the only other supported value is `"CREATE_NEVER"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs#configuration.query.createDisposition}{the API documentation}
#'   for more information
#' @param write_disposition behavior for writing data.
#'   defaults to `"WRITE_EMPTY"`, other possible values are
#'   `"WRITE_TRUNCATE"` and `"WRITE_APPEND"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs#configuration.query.writeDisposition}{the API documentation}
#'   for more information
#' @param default_dataset (optional) default dataset for any table references in
#'   `query`, either as a string in the format used by BigQuery or as a
#'   list with `project_id` and `dataset_id` entries
#' @param use_legacy_sql (optional) set to `FALSE` to enable BigQuery's standard SQL.
#' @family jobs
#' @return a job resource list, as documented at
#'   \url{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs}
#' @seealso API documentation for insert method:
#'   \url{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs/insert}
#' @export
#' @keywords internal
insert_query_job <- function(query, project,
                             destination_table = NULL,
                             default_dataset = NULL,
                             create_disposition = "CREATE_IF_NEEDED",
                             write_disposition = "WRITE_EMPTY",
                             use_legacy_sql = TRUE,
                             ...) {

  .Deprecated("bq_perform_query", package = "bigrquery")

  assert_that(is.string(project), is.string(query))

  url <- bq_path(project, jobs = "")
  body <- list(
    configuration = list(
      query = list(
        query = query,
        useLegacySql = use_legacy_sql
      )
    )
  )

  if (!is.null(destination_table)) {
    if (is.character(destination_table)) {
      destination_table <- parse_table(destination_table, project_id = project)
    }
    assert_that(
      is.string(destination_table$project_id),
      is.string(destination_table$dataset_id),
      is.string(destination_table$table_id)
    )
    body$configuration$query$allowLargeResults <- TRUE
    body$configuration$query$destinationTable <- list(
      projectId = destination_table$project_id,
      datasetId = destination_table$dataset_id,
      tableId = destination_table$table_id
    )
    body$configuration$query$createDisposition <- create_disposition
    body$configuration$query$writeDisposition <- write_disposition
  }

  if (!is.null(default_dataset)) {
    if (is.character(default_dataset)) {
      default_dataset <- parse_dataset(default_dataset, project_id = project)
    }
    assert_that(
      is.string(default_dataset$project_id),
      is.string(default_dataset$dataset_id)
    )
    body$configuration$query$defaultDataset <- list(
      projectId = default_dataset$project_id,
      datasetId = default_dataset$dataset_id
    )
  }

  bq_post(url, body = bq_body(body, ...))
}
