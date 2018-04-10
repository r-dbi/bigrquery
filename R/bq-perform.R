#' Perform jobs
#'
#' @description
#' These functions are low-level functions designed to be used by experts.
#' Each of these low-level functions is paired with a high-level function that
#' you should use instead:
#'
#' * `bq_perform_copy()`:    [bq_table_copy()].
#' * `bq_perform_extract()`: [bq_table_extract()].
#' * `bq_perform_query()`:   [bq_dataset_query()], [bq_project_query()].
#' * `bq_perform_upload()`:  [bq_table_upload()].
#'
#' @return A [bq_job].
#' @examples
#' if (bq_testable()) {
#' ds <- bq_test_dataset()
#' bq_mtcars <- bq_table(ds, "mtcars")
#' job <- bq_perform_upload(bq_mtcars, mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' bq_job_wait(job)
#' bq_table_exists(bq_mtcars)
#' head(bq_table_download(bq_mtcars))
#' }
#' @name perform
NULL

#' @export
#' @name perform
bq_perform_extract <- function(x,
                                  destination_uris,
                                  compression = "NONE",
                                  destination_format = "NEWLINE_DELIMITED_JSON",
                                  ...,
                                  print_header = TRUE,
                                  billing = x$project) {
  x <- as_bq_table(x)
  assert_that(
    is.character(destination_uris),
    is.string(billing)
  )

  url <- bq_path(billing, jobs = "")
  body <- list(
    configuration = list(
      extract = list(
        sourceTable = tableReference(x),
        destinationUris = destination_uris,
        destinationFormat = unbox(destination_format),
        compression = unbox(compression),
        printHeader = unbox(print_header)
      )
    )
  )

  res <- bq_post(
    url,
    body = bq_body(body, ...),
    query = list(field = "jobReference")
  )
  bq_job(res$jobReference$projectId, res$jobReference$jobId)
}

#' @export
#' @name perform
#' @param values Data frame of values to insert.
bq_perform_upload <- function(x, values,
                              create_disposition = "CREATE_IF_NEEDED",
                              write_disposition = "WRITE_APPEND",
                              ...,
                              billing = x$project
                              ) {

  x <- as_bq_table(x)
  assert_that(
    is.data.frame(values),
    is.string(billing)
  )

  config <- list(
    configuration = list(
      load = list(
        sourceFormat = unbox("NEWLINE_DELIMITED_JSON"),
        schema = list(fields = schema_fields(values)),
        destinationTable = tableReference(x),
        createDisposition = unbox(create_disposition),
        writeDisposition = unbox(write_disposition)
      )
    )
  )
  config <- bq_body(config, ...)
  config_part <- part(
    c("Content-type" = "application/json; charset=UTF-8"),
    jsonlite::toJSON(config, pretty = TRUE)
  )

  data_part <- part(
    c("Content-type" = "application/json; charset=UTF-8"),
    export_json(values)
  )

  url <- bq_path(billing, jobs = "")
  res <- bq_upload(
    url,
    parts = c(config_part, data_part),
    query = list(fields = "jobReference")
  )
  bq_job(res$jobReference$projectId, res$jobReference$jobId)
}

#' @export
#' @rdname perform
#' @param SQL query string.
#' @param destination_table A [bq_table] where results should be stored.
#'   If not supplied, results will be saved to a temporary table that lives
#'   in a special dataset. You must supply this parameter for large
#'   queries (> 128 MB compressed).
#' @param create_disposition Behavior if `destination_table` does not exist.
#'   Can be `"CREATE_IF_NEEDED"` or `"CREATE_NEVER"`; see
#'   [API documentation](https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.load.createDisposition})
#'   for more information.
#' @param write_disposition Behavior if `destination_table` already exists.
#'   Possible values are `"WRITE_APPEND"`, `"WRITE_TRUNCATE"` and
#'   `"WRITE_EMPTY"`. See
#'   [API documentation[(https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.load.writeDisposition)
#'   for more information.
bq_perform_query <- function(query, billing,
                             ...,
                             destination_table = NULL,
                             default_dataset = NULL,
                             create_disposition = "CREATE_IF_NEEDED",
                             write_disposition = "WRITE_EMPTY",
                             use_legacy_sql = FALSE
                             ) {
  assert_that(is.string(query), is.string(billing))

  query <- list(
    query = unbox(query),
    useLegacySql = unbox(use_legacy_sql)
  )

  if (!is.null(destination_table)) {
    query$destinationTable <- tableReference(destination_table)
    query$createDisposition <- unbox(create_disposition)
    query$writeDisposition <- unbox(write_disposition)
    if (use_legacy_sql)
      query$allowLargeResults <- unbox(TRUE)
  }

  if (!is.null(default_dataset)) {
    query$defaultDataset <- datasetReference(default_dataset)
  }

  url <- bq_path(billing, jobs = "")
  body <- list(configuration = list(query = query))

  res <- bq_post(
    url,
    body = bq_body(body, ...),
    query = list(fields = "jobReference")
  )
  bq_job(res$jobReference$projectId, res$jobReference$jobId)
}

#' @export
#' @rdname perform
bq_perform_copy <- function(src, dest,
                            create_disposition = "CREATE_IF_NEEDED",
                            write_disposition = "WRITE_EMPTY",
                            ...,
                            billing = NULL,
                            quiet = NA) {

  billing <- billing %||% dest$project
  url <- bq_path(billing, jobs = "")

  body <- list(
    configuration = list(
      copy = list(
        sourceTable = tableReference(src),
        destinationTable = tableReference(dest),
        createDisposition = unbox(create_disposition),
        writeDisposition = unbox(write_disposition)
      )
    )
  )

  res <- bq_post(
    url,
    body = bq_body(body, ...),
    query = list(fields = "jobReference")
  )
  bq_job(res$jobReference$projectId, res$jobReference$jobId)
}
