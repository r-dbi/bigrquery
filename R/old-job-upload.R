#' Create a new upload job [deprecated]
#'
#' Please use [api-job] instead.
#'
#' @inheritParams get_table
#' @param project,dataset Project and dataset identifiers
#' @param table name of table to insert values into
#' @param values data frame of data to upload
#' @param billing project ID to use for billing
#' @param create_disposition behavior for table creation if the destination
#'   already exists. defaults to `"CREATE_IF_NEEDED"`,
#'   the only other supported value is `"CREATE_NEVER"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs#configuration.load.createDisposition}{the API documentation}
#'   for more information
#' @param write_disposition behavior for writing data if the destination already
#'   exists. defaults to `"WRITE_APPEND"`, other possible values are
#'   `"WRITE_TRUNCATE"` and `"WRITE_EMPTY"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs#configuration.load.writeDisposition}{the API documentation}
#'   for more information
#' @inheritParams bq_dataset_create
#' @seealso Google API documentation:
#' \url{https://cloud.google.com/bigquery/docs/loading-data}
#' @family jobs
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' list_datasets(bq_test_project)
#' list_tables("193487687779", "houston")
#' job <- insert_upload_job("193487687779", "houston", "mtcars", mtcars)
#' wait_for(job)
#' list_tables("193487687779", "houston")
#' delete_table("193487687779", "houston", "mtcars")
#' }
insert_upload_job <- function(project, dataset, table, values,
                              billing = project,
                              create_disposition = "CREATE_IF_NEEDED",
                              write_disposition = "WRITE_APPEND",
                              ...) {

  .Deprecated("bq_perform_upload", package = "bigrquery")

  assert_that(
    is.string(project),
    is.string(dataset),
    is.string(table),
    is.data.frame(values),
    is.string(billing)
  )

  # https://developers.google.com/bigquery/docs/reference/v2/jobs#resource
  config <- list(
    configuration = list(
      load = list(
        sourceFormat = "NEWLINE_DELIMITED_JSON",
        schema = list(
          fields = as_json(as_bq_fields(values))
        ),
        destinationTable = list(
          projectId = project,
          datasetId = dataset,
          tableId = table
        ),
        createDisposition = create_disposition,
        writeDisposition = write_disposition
      )
    )
  )
  config <- bq_body(config, ...)
  config_part <- part(
    c("Content-type" = "application/json; charset=UTF-8"),
    jsonlite::toJSON(config, auto_unbox = TRUE)
  )
  data_part <- part(
    c("Content-type" = "application/json; charset=UTF-8"),
    export_json(values)
  )

  url <- bq_path(billing, jobs = "")
  bq_upload(url, c(config_part, data_part))
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
  on.exit(close(con))
  jsonlite::stream_out(values, con, verbose = FALSE, na = "null")

  rawToChar(rawConnectionValue(con))
}
