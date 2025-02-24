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
#' @section Google BigQuery API documentation:
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
#' @examplesIf bq_testable()
#' ds <- bq_test_dataset()
#' bq_mtcars <- bq_table(ds, "mtcars")
#' job <- bq_perform_upload(bq_mtcars, mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' bq_job_wait(job)
#' bq_table_exists(bq_mtcars)
#' head(bq_table_download(bq_mtcars))
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
#' @param destination_format The exported file format:
#'   * For CSV files, specify "CSV" (Nested and repeated data is not supported).
#'   * For newline-delimited JSON, specify "NEWLINE_DELIMITED_JSON".
#'   * For Avro, specify "AVRO".
#'   * For parquet, specify "PARQUET".
#' @param compression The compression type to use for exported files:
#'   * For CSV files: "GZIP" or "NONE".
#'   * For newline-delimited JSON: "GZIP" or "NONE".
#'   * For Avro: "DEFLATE", "SNAPPY" or "NONE".
#'   * For parquet: "SNAPPY", "GZIP", "ZSTD" or "NONE".
#' @param ... Additional arguments passed on to the underlying API call.
#'   snake_case names are automatically converted to camelCase.
#' @param print_header Whether to print out a header row in the results.
#' @param billing Identifier of project to bill.
bq_perform_extract <- function(
  x,
  destination_uris,
  destination_format = "NEWLINE_DELIMITED_JSON",
  compression = "NONE",
  ...,
  print_header = TRUE,
  billing = x$project
) {
  x <- as_bq_table(x)
  destination_uris <- as.character(destination_uris) # for gs_object
  check_string(destination_format)
  check_string(compression)
  check_bool(print_header)
  check_string(billing)

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
#' @param source_format The format of the data files:
#'   * For newline-delimited JSON, specify "NEWLINE_DELIMITED_JSON".
#'   * For parquet, specify "PARQUET".
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
bq_perform_upload <- function(
  x,
  values,
  fields = NULL,
  source_format = c("NEWLINE_DELIMITED_JSON", "PARQUET"),
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_EMPTY",
  ...,
  billing = x$project
) {
  x <- as_bq_table(x)
  if (!is.data.frame(values)) {
    cli::cli_abort("{.arg values} must be a data frame.")
  }
  fields <- as_bq_fields(fields)
  source_format <- arg_match(source_format)
  check_string(create_disposition)
  check_string(write_disposition)
  check_string(billing)

  load <- list(
    sourceFormat = unbox(source_format),
    destinationTable = tableReference(x),
    createDisposition = unbox(create_disposition),
    writeDisposition = unbox(write_disposition)
  )

  if (!is.null(fields)) {
    load$schema <- list(fields = as_json(fields))
  }
  if (!bq_table_exists(x)) {
    load$autodetect <- unbox(TRUE)
  }

  metadata <- list(configuration = list(load = load))
  metadata <- bq_body(metadata, ...)
  metadata <- list(
    "type" = "application/json; charset=UTF-8",
    "content" = jsonlite::toJSON(metadata, pretty = TRUE)
  )

  if (source_format == "NEWLINE_DELIMITED_JSON") {
    media <- list(
      "type" = "application/json; charset=UTF-8",
      "content" = export_json(values)
    )
  } else {
    # https://cloud.google.com/bigquery/docs/loading-data-cloud-storage-parquet?hl=es-419
    media <- list(
      "type" = "application/vnd.apache.parquet",
      "content" = nanoparquet::write_parquet(values, ":raw:")
    )
  }

  url <- bq_path(billing, jobs = "")
  res <- bq_upload(
    url,
    metadata,
    media,
    query = list(fields = "jobReference")
  )
  as_bq_job(res$jobReference)
}

# https://cloud.google.com/bigquery/docs/loading-data-cloud-storage-json#details_of_loading_json_data
export_json <- function(values) {
  # Eliminate row names
  rownames(values) <- NULL

  # Convert times to canonical format
  is_time <- vapply(values, function(x) inherits(x, "POSIXt"), logical(1))
  values[is_time] <- lapply(values[is_time], format, "%Y-%m-%d %H:%M:%S")

  # Convert wk_wkt to text
  is_wk <- vapply(values, function(x) inherits(x, "wk_vctr"), logical(1))
  values[is_wk] <- lapply(values[is_wk], as.character)

  # Unbox blobs
  is_blob <- vapply(values, function(x) inherits(x, "blob"), logical(1))
  values[is_blob] <- lapply(values[is_blob], function(x) {
    vapply(x, jsonlite::base64_enc, character(1))
  })

  con <- rawConnection(raw(0), "r+")
  defer(close(con))
  jsonlite::stream_out(values, con, verbose = FALSE, na = "null")

  rawToChar(rawConnectionValue(con))
}

#' @export
#' @name api-perform
#' @param source_uris The fully-qualified URIs that point to your data in
#'   Google Cloud.
#'
#'   For Google Cloud Storage URIs: Each URI can contain one
#'   `'*'` wildcard character and it must come after the 'bucket' name.
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
bq_perform_load <- function(
  x,
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
  check_string(billing)
  check_string(source_format)
  check_number_decimal(nskip, min = 0)
  check_string(create_disposition)
  check_string(write_disposition)

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
bq_perform_query <- function(
  query,
  billing,
  ...,
  parameters = NULL,
  destination_table = NULL,
  default_dataset = NULL,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_EMPTY",
  use_legacy_sql = FALSE,
  priority = "INTERACTIVE"
) {
  query <- as_query(query)
  check_string(billing)

  check_string(create_disposition)
  check_string(write_disposition)
  check_bool(use_legacy_sql)
  check_string(priority)

  query <- list(
    query = unbox(query),
    useLegacySql = unbox(use_legacy_sql),
    priority = unbox(priority)
  )

  if (length(parameters) > 0) {
    parameters <- as_bq_params(parameters)
    query$queryParameters <- as_json(parameters)
  }

  if (!is.null(destination_table)) {
    query$destinationTable <- tableReference(destination_table)
    query$createDisposition <- unbox(create_disposition)
    query$writeDisposition <- unbox(write_disposition)
    if (use_legacy_sql) {
      query$allowLargeResults <- unbox(TRUE)
    }
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
bq_perform_query_dry_run <- function(
  query,
  billing,
  ...,
  default_dataset = NULL,
  parameters = NULL,
  use_legacy_sql = FALSE
) {
  query <- bq_perform_query_data(
    query = query,
    default_dataset = default_dataset,
    parameters = parameters,
    use_legacy_sql = use_legacy_sql
  )

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
bq_perform_query_schema <- function(
  query,
  billing,
  ...,
  default_dataset = NULL,
  parameters = NULL
) {
  query <- bq_perform_query_data(
    query = query,
    default_dataset = default_dataset,
    parameters = parameters,
    use_legacy_sql = FALSE
  )

  url <- bq_path(billing, jobs = "")
  body <- list(configuration = list(query = query, dryRun = unbox(TRUE)))

  res <- bq_post(
    url,
    body = bq_body(body, ...),
    query = list(fields = "statistics")
  )
  # https://cloud.google.com/bigquery/docs/reference/rest/v2/tables#TableSchema
  res$statistics$query$schema$fields
}

bq_perform_query_data <- function(
  query,
  ...,
  default_dataset = NULL,
  parameters = NULL,
  use_legacy_sql = FALSE,
  call = caller_env()
) {
  check_string(query, error_call = call)
  check_bool(use_legacy_sql, error_call = call)

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

  query
}


#' @export
#' @rdname api-perform
bq_perform_copy <- function(
  src,
  dest,
  create_disposition = "CREATE_IF_NEEDED",
  write_disposition = "WRITE_EMPTY",
  ...,
  billing = NULL
) {
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
