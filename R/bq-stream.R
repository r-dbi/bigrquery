#' Streams data from data.from into BigQUery
#'
#' @param x bq_table where values will be streamed to
#' @param values data.frame that will be streamed
#' @param skip_invalid_rows
#'   (optional) Insert all valid rows of a request,
#'   even if invalid rows exist. The default value is False, which
#'   causes the entire request to fail if any invalid rows exist.
#' @param ignore_unknown_values
#'   (optional) Accept rows that contain values that do not match the
#'   schema. The unknown values are ignored. Default is False, which
#'   treats unknown values as errors.
#' @param template_suffix
#'   (optional) treat `name` as a template table and provide a suffix.
#'   BigQuery will create the table `<name> + <template_suffix>` based
#'   on the schema of the template table. See
#'   https://cloud.google.com/bigquery/streaming-data-into-bigquery#template-tables
#' @export
#' @rdname bq-streaming
#' @seealso https://cloud.google.com/bigquery/streaming-data-into-bigquery
bq_table_stream <- function(x, values, skip_invalid_rows = "false", ignore_unknown_values = "false", template_suffix = NULL) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  url <- paste0(url, "/", "insertAll")
  body <- bq_stream_body(
    x = values,
    skip_invalid_rows = skip_invalid_rows,
    ignore_unknown_values = ignore_unknown_values,
    template_suffix = template_suffix
  )
  print(jsonlite::toJSON(body, pretty = TRUE, auto_unbox = TRUE))
  bq_post(url, body)
}

#' Converts data.table to streaming request body
#'
#' @noRd
#' @return list that represents json for streaming
#' @seealso https://cloud.google.com/bigquery/docs/reference/rest/v2/tabledata/insertAll#InsertionRow
bq_stream_body <- function(x, skip_invalid_rows = "false", ignore_unknown_values = "false", template_suffix = NULL) {
  l <- setNames(split(x, seq(nrow(x))), rownames(x))
  rows <- lapply(l, function(row) {
    list(
      insertId = paste(sample(c(letters[1:6], 0:9), 30, replace = TRUE), collapse = ""),
      json = as.list(row)
    )
  })
  names(rows) <- NULL
  body <- list(
    kind = "bigquery#tableDataInsertAllRequest",
    skipInvalidRows = skip_invalid_rows,
    ignoreUnknownValues = ignore_unknown_values,
    rows = rows
  )

  if (!is.null(template_suffix)) {
    body["templateSuffix"] <- template_suffix
  }
  body
}
