#' Upload data.
#'
#' This sends all of the data inline in the HTTP request so is only suitable
#' for relatively small datasets.
#'
#' @inheritParams get_table
#' @param table name of table to insert values into
#' @param values data frame of data to upload
#' @param billing project ID to use for billing
#' @param create_disposition behavior for table creation if the destination
#'   already exists. defaults to `"CREATE_IF_NEEDED"`,
#'   the only other supported value is `"CREATE_NEVER"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.load.createDisposition}{the API documentation}
#'   for more information
#' @param write_disposition behavior for writing data if the destination already
#'   exists. defaults to `"WRITE_APPEND"`, other possible values are
#'   `"WRITE_TRUNCATE"` and `"WRITE_EMPTY"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.load.writeDisposition}{the API documentation}
#'   for more information
#' @inheritParams insert_dataset
#' @seealso Google API documentation:
#' \url{https://developers.google.com/bigquery/loading-data-into-bigquery#loaddatapostrequest}
#' @family jobs
#' @export
#' @examples
#' \dontrun{
#' list_datasets("193487687779")
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
          fields = schema_fields(values)
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

schema_fields <- function(data) {
  types <- vapply(data, data_type, character(1))
  unname(Map(function(type, name) list(name = name, type = type), types, names(data)))
}

data_type <- function(x) {
  if (is.factor(x)) return("STRING")
  if (inherits(x, "POSIXt")) return("TIMESTAMP")
  if (inherits(x, "hms")) return("TIME")
  if (inherits(x, "Date")) return("DATE")

  switch(
    typeof(x),
    character = "STRING",
    logical = "BOOLEAN",
    double = "FLOAT",
    integer = "INTEGER",
    stop("Unsupported type: ", typeof(x), call. = FALSE)
  )
}

export_json <- function(values) {
  # Eliminate row names
  rownames(values) <- NULL

  # Convert times to unix timestamps
  is_time <- vapply(values, function(x) inherits(x, "POSIXt"), logical(1))
  values[is_time] <- lapply(values[is_time], as.numeric)

  con <- rawConnection(raw(0), "r+")
  on.exit(close(con))
  jsonlite::stream_out(values, con, verbose = FALSE, na = "null")

  rawToChar(rawConnectionValue(con))
}
