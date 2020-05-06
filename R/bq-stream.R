#' Stream data.frame into into BigQUery
#'
#' @param x bq_table where values will be streamed to
#' @param values data.frame that will be streamed
#' @param insert_ids
#'   (optional) Unique ids, one per row being inserted.
#'   If omitted, unique IDs are created.
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
#' @return NULL if there are no errors, otherwise list of errors with reasons
#' @export
#' @rdname bq-streaming
#' @seealso https://cloud.google.com/bigquery/streaming-data-into-bigquery
bq_table_stream <- function(x,
                            values,
                            insert_ids = bq_stream_random_id(nrow(values)),
                            skip_invalid_rows = "false",
                            ignore_unknown_values = "false",
                            template_suffix = NULL) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  url <- paste0(url, "/", "insertAll")
  body <- bq_stream_body(
    x = values,
    insert_ids = insert_ids,
    skip_invalid_rows = skip_invalid_rows,
    ignore_unknown_values = ignore_unknown_values,
    template_suffix = template_suffix
  )

  response <- bq_post(url, body)
  invisible(response$insertErrors)
}

#' Converts data.frame to streaming request body
#'
#' @noRd
#' @return list that represents json for streaming
#' @seealso https://cloud.google.com/bigquery/docs/reference/rest/v2/tabledata/insertAll#InsertionRow
bq_stream_body <- function(x,
                           insert_ids,
                           skip_invalid_rows,
                           ignore_unknown_values,
                           template_suffix
                           ) {


  rows <- lapply(1:nrow(x), function(row.index) {
    list(
      insertId = insert_ids[row.index],
      json = as.list(x[row.index, ])
    )
  })

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

#' Generates random string that BigQuery uses do de-duplicate records
#'
#' @noRd
bq_stream_random_id <- function(n = 1) {
 assert_that(n > 0)
 vapply(
   1:n,
   FUN = function(x) paste(sample(c(letters[1:6], 0:9), 30, replace = TRUE), collapse = ""),
   FUN.VALUE = character(1)
  )
}
