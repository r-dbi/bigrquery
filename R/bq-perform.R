#' BigQuery jobs: perform a job
#'
#' @description
#' These functions are low-level functions designed to be used by experts.
#' Each of these low-level functions is paired with a high-level function that
#' you should use instead:
#'
#' * `bq_perform_copy()`:    [bq_table_copy()].
#' * `bq_perform_query()`:   [bq_dataset_query()], [bq_project_query()].
#' * `bq_perform_upload()`:  [bq_table_upload()].
#' * `bq_perform_load()`:    [bq_table_load()].
#' * `bq_perform_extract()`: [bq_table_save()].
#'
#' @section API documentation:
#' * [jobs](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs)
#'
#' Additional information at:
#' * [exporting data](https://cloud.google.com/bigquery/docs/exporting-data)
#' * [loading data](https://cloud.google.com/bigquery/docs/loading-data)
#' * [writing queries](https://cloud.google.com/bigquery/docs/writing-results)
#' * [copying a table](https://cloud.google.com/bigquery/docs/managing-tables#copy-table)
#'
#' @return A [bq_job].
#' @keywords internal
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
#' @name api-perform
NULL

#' @export
#' @name api-perform
#' @param x A [bq_table]
#' @param destination_uris A character vector of fully-qualified Google Cloud
#'   Storage URIs where the extracted table should be written. Can export
#'   up to 1 Gb of data per file. Use a wild card URI (e.g.
#'   `gs://[YOUR_BUCKET]/file-name-*.json`) to automatically create any
#'   number of files.
#' @param destination_format The exported file format. Possible values
#'   include "CSV", "NEWLINE_DELIMITED_JSON" and "AVRO". Tables with nested or
#'   repeated fields cannot be exported as CSV.
#' @param compression The compression type to use for exported files. Possible
#'   values include "GZIP", "DEFLATE", "SNAPPY", and "NONE". "DEFLATE" and
#'   "SNAPPY" are only supported for Avro.
#' @param ... Additional arguments passed on to the underlying API call.
#'   snake_case names are automatically converted to camelCase.
#' @param print_header Whether to print out a header row in the results.
#' @param billing Identifier of project to bill.
bq_perform_extract <- function(x,
                               destination_uris,
                               destination_format = "NEWLINE_DELIMITED_JSON",
                               compression = "NONE",
                               ...,
                               print_header = TRUE,
                               billing = x$project) {
  x <- as_bq_table(x)
  destination_uris <- as.character(destination_uris)
  assert_that(is.string(billing))

  url <- bq_path(billing, jobs = "")
  body <- list(
    configuration = list(
      extract = list(
        sourceTable = tableReference(x),
        destinationUris = as.list(destination_uris),
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
  as_bq_job(res$jobReference)
}

#' @export
#' @name api-perform
#' @param values Data frame of values to insert.
#' @param create_disposition Specifies whether the job is allowed to create
#'   new tables.
#'
#'   The following values are supported:
#'   * "CREATE_IF_NEEDED": If the table does not exist, BigQuery creates the
#'     table.
#'   * "CREATE_NEVER": The table must already exist. If it does not, a
#'     'notFound' error is returned in the job result.
#' @param write_disposition Specifies the action that occurs if the
#'   destination table already exists. The following values are supported:
#'
#'   * "WRITE_TRUNCATE": If the table already exists, BigQuery overwrites the
#'     table data.
#'   * "WRITE_APPEND": If the table already exists, BigQuery appends the data
#'     to the table.
#'   * "WRITE_EMPTY": If the table already exists and contains data, a
#'     'duplicate' error is returned in the job result.
bq_perform_upload <- function(x, values,
                              fields = NULL,
                              create_disposition = "CREATE_IF_NEEDED",
                              write_disposition = "WRITE_EMPTY",
                              ...,
                              billing = x$project
                              ) {

  x <- as_bq_table(x)
  assert_that(
    is.data.frame(values),
    is.string(billing)
  )

  load <- list(
    sourceFormat = unbox("NEWLINE_DELIMITED_JSON"),
    destinationTable = tableReference(x),
    createDisposition = unbox(create_disposition),
    writeDisposition = unbox(write_disposition)
  )

  if (!is.null(fields)) {
    fields <- as_bq_fields(fields)
    load$schema <- list(fields = as_json(fields))
  } else if (!bq_table_exists(x)) {
    load$autodetect <- unbox(TRUE)
  }

  config <- list(configuration = list(load = load))
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
  as_bq_job(res$jobReference)
}


#' @export
#' @name api-perform
#' @param source_uris The fully-qualified URIs that point to your data in
#'   Google Cloud.
#'
#'   For Google Cloud Storage URIs: Each URI can contain one
#'   `'*'`` wildcard character and it must come after the 'bucket' name.
#'   Size limits related to load jobs apply to external data sources.
#'
#'   For Google Cloud Bigtable URIs: Exactly one URI can be specified and
#'   it has be a fully specified and valid HTTPS URL for a Google Cloud
#'    Bigtable table. For Google Cloud Datastore backups: Exactly one URI
#'  can be specified. Also, the '*' wildcard character is not allowed.
#' @param source_format The format of the data files:
#'   * For CSV files, specify "CSV".
#'   * For datastore backups, specify "DATASTORE_BACKUP".
#'   * For newline-delimited JSON, specify "NEWLINE_DELIMITED_JSON".
#'   * For Avro, specify "AVRO".
#'   * For parquet, specify "PARQUET".
#'   * For orc, specify "ORC".
#' @param fields A [bq_fields] specification, or something coercible to it
#'   (like a data frame). Leave as `NULL` to allow BigQuery to auto-detect
#'   the fields.
#' @param nskip For `source_format = "CSV"`, the number of header rows to skip.
bq_perform_load <- function(x,
                            source_uris,
                            billing = x$project,
                            source_format = "NEWLINE_DELIMITED_JSON",
                            fields = NULL,
                            nskip = 0,
                            create_disposition = "CREATE_IF_NEEDED",
                            write_disposition = "WRITE_EMPTY",
                            ...
                            ) {
  x <- as_bq_table(x)
  source_uris <- as.character(source_uris)
  assert_that(is.string(billing))

  load <- list(
    sourceUris = as.list(source_uris),
    sourceFormat = unbox(source_format),
    destinationTable = tableReference(x),
    createDisposition = unbox(create_disposition),
    writeDisposition = unbox(write_disposition)
  )

  if (source_format == "CSV") {
    load$skipLeadingRows <- nskip
  }

  if (!is.null(fields)) {
    fields <- as_bq_fields(fields)
    load$schema <- list(fields = as_json(fields))
  } else {
    load$autodetect <- TRUE
  }

  body <- list(configuration = list(load = load))

  url <- bq_path(billing, jobs = "")
  res <- bq_post(
    url,
    body = bq_body(body, ...),
    query = list(fields = "jobReference")
  )
  as_bq_job(res$jobReference)
}

#' @export
#' @rdname api-perform
#' @param query SQL query string.
#' @param parameters Named list of parameters match to query parameters.
#'   Parameter `x` will be matched to placeholder `@x`.
#'
#'   Generally, you can supply R vectors and they will be automatically
#'   converted to the correct type. If you need greater control, you can call
#'   [bq_param_scalar()] or [bq_param_array()] explicitly.
#'
#'   See <https://cloud.google.com/bigquery/docs/parameterized-queries>
#'   for more details.
#' @param destination_table A [bq_table] where results should be stored.
#'   If not supplied, results will be saved to a temporary table that lives
#'   in a special dataset. You must supply this parameter for large
#'   queries (> 128 MB compressed).
#' @param priority Specifies a priority for the query. Possible values include
#'   "INTERACTIVE" and "BATCH". Batch queries do not start immediately,
#'   but are not rate-limited in the same way as interactive queries.
#' @param default_dataset A [bq_dataset] used to automatically qualify table names.
#' @param use_legacy_sql If `TRUE` will use BigQuery's legacy SQL format.
bq_perform_query <- function(query, billing,
                             ...,
                             parameters = NULL,
                             destination_table = NULL,
                             default_dataset = NULL,
                             create_disposition = "CREATE_IF_NEEDED",
                             write_disposition = "WRITE_EMPTY",
                             use_legacy_sql = FALSE,
                             priority = "INTERACTIVE"
                             ) {
  assert_that(is.string(query), is.string(billing))

  query <- list(
    query = unbox(query),
    useLegacySql = unbox(use_legacy_sql),
    priority = unbox(priority)
  )

  if (!is.null(parameters)) {
    parameters <- as_bq_params(parameters)
    query$queryParameters <- as_json(parameters)
  }

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
  as_bq_job(res$jobReference)
}

#' @export
#' @rdname api-perform
bq_perform_query_dry_run <- function(query, billing,
                                     ...,
                                     default_dataset = NULL,
                                     parameters = NULL,
                                     use_legacy_sql = FALSE) {

  assert_that(is.string(query), is.string(billing))

  query <- list(
    query = unbox(query),
    useLegacySql = unbox(use_legacy_sql)
  )
  if (!is.null(parameters)) {
    parameters <- as_bq_params(parameters)
    query$queryParameters <- as_json(parameters)
  }
  if (!is.null(default_dataset)) {
    query$defaultDataset <- datasetReference(default_dataset)
  }

  url <- bq_path(billing, jobs = "")
  body <- list(configuration = list(query = query, dryRun = unbox(TRUE)))

  res <- bq_post(
    url,
    body = bq_body(body, ...),
    query = list(fields = "statistics")
  )
  bytes <- as.numeric(res$statistics$query$totalBytesProcessed)
  structure(bytes, class = "bq_bytes")
}

#' @export
#' @rdname api-perform
bq_perform_copy <- function(src, dest,
                            create_disposition = "CREATE_IF_NEEDED",
                            write_disposition = "WRITE_EMPTY",
                            ...,
                            billing = NULL) {

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
  as_bq_job(res$jobReference)
}

