#' Upload data.
#'
#' This sends all of the data inline in the HTTP request so is only suitable
#' for relatively small datasets.
#'
#' @inheritParams get_table
#' @param table name of table to insert values into
#' @param values data frame of data to upload
#' @param billing project ID to use for billing
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
insert_upload_job <- function(project, dataset, table, values, billing = project) {
  assert_that(is.string(project), is.string(dataset), is.string(table),
    is.data.frame(values), is.string(billing))

  # https://developers.google.com/bigquery/docs/reference/v2/jobs#resource
  config <- list(
    configuration = list(
      load = list(
        sourceFormat = "CSV",
        schema = list(
          fields = schema_fields(values)
        ),
        destinationTable = list(
          projectId = project,
          datasetId = dataset,
          tableId = table
        )
      )
    )
  )
  config_part <- part(c("Content-type" = "application/json; charset=UTF-8"),
    jsonlite::toJSON(config, pretty = TRUE))

  csv <- standard_csv(values)
  csv_part <- part(c("Content-type" = "application/octet-stream"), csv)

  url <- sprintf("projects/%s/jobs", billing)
  bq_upload(url, c(config_part, csv_part))
}

schema_fields <- function(data) {
  types <- vapply(data, data_type, character(1))
  unname(Map(function(type, name) list(name = name, type = type), types, names(data)))
}

data_type <- function(x) {
  switch(class(x)[1],
    character = "STRING",
    logical = "BOOLEAN",
    numeric = "FLOAT",
    integer = "INTEGER",
    factor = "STRING",
    Date = "TIMESTAMP",
    POSIXct = "TIMESTAMP",
    stop("Unknown class ", paste0(class(x), collapse = "/"))
  )
}

standard_csv <- function(values) {
  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)

  # Encode special characters in strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], encodeString, na.encode = FALSE)

  # Encode dates and times
  is_time <- vapply(values, function(x) inherits(x, "POSIXct"), logical(1))
  values[is_time] <- lapply(values[is_time], as.numeric)

  is_date <- vapply(values, function(x) inherits(x, "Date"), logical(1))
  values[is_date] <- lapply(values[is_date], function(x) as.numeric(as.POSIXct(x)))

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  conn <- file(tmp, open = "wb")
  write.table(values, conn, sep = ",", na = "", qmethod = "double",
              row.names = FALSE, col.names = FALSE, eol = "\12")
  close(conn)

  # Don't read trailing nl
  readChar(tmp, file.info(tmp)$size - 1, useBytes = TRUE)
}
