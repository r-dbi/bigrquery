#' Stream data.frame into BigQUery
#'
#' @param x bq_table where values will be streamed to
#' @param values data.frame that will be streamed
#' @param insert_ids
#'   (optional) Unique ids, one per row being inserted.
#'   If omitted, unique IDs are created.
#' @param skip_invalid_rows
#'   (optional) Insert all valid rows of a request,
#'   even if invalid rows exist. The default value is FALSE, which
#'   causes the entire request to fail if any invalid rows exist.
#' @param ignore_unknown_values
#'   (optional) Accept rows that contain values that do not match the
#'   schema. The unknown values are ignored. Default is FALSE, which
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
#' @examples
#' \dontrun{
#' ds <- bq_test_dataset()
#' tb <- bq_table(ds, "cars")
#' # create an empty table with schema for `cars` data.frame
#' bq_table_create(tb, cars)
#'
#' bq_table_stream(tb, cars))
#'}
bq_table_stream <- function(x,
                            values,
                            insert_ids = NULL,
                            skip_invalid_rows = FALSE,
                            ignore_unknown_values = FALSE,
                            template_suffix = NULL) {
  x <- as_bq_table(x)
  url <- paste0(bq_path(x$project, x$dataset, x$table), "/", "insertAll")
  body <- bq_stream_body(
    x = values,
    insert_ids = insert_ids,
    skip_invalid_rows = skip_invalid_rows,
    ignore_unknown_values = ignore_unknown_values,
    template_suffix = template_suffix
  )

  response <- bq_post(url, body)
  response$insertErrors
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
    row <- list(
      insertId = get_insert_id(insert_ids, row.index),
      json = as.list(x[row.index, ])
    )
    Filter(Negate(is.null), row)
  })

  body <- list(
    kind = "bigquery#tableDataInsertAllRequest",
    skipInvalidRows = skip_invalid_rows,
    ignoreUnknownValues = ignore_unknown_values,
    rows = rows
  )

  if (!is.null(template_suffix)) {
    body$templateSuffix <- template_suffix
  }
  body
}

get_insert_id <- function(x, i) {
  if (is.null(x)) {
    NULL
  }
  else {
    x[i]
  }
}
