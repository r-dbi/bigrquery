#' BigQuery tables
#'
#' Basic create-read-update-delete verbs for tables, as well as functions
#' for uploading and downloading data in to/from memory (`bq_table_upload()`,
#' (`bq_table_download()`)), and saving to/loading from Google CloudStorage
#' (`bq_table_load()`, `bq_table_save()`).
#'
#' @param x A [bq_table], or an object coercible to a `bq_table`.
#' @inheritParams api-job
#' @inheritParams api-perform
#' @inheritParams bq_projects
#' @section API documentation:
#' * [insert](https://cloud.google.com/bigquery/docs/reference/rest/v2/tables/insert)
#' * [get](https://cloud.google.com/bigquery/docs/reference/rest/v2/tables/get)
#' * [delete](https://cloud.google.com/bigquery/docs/reference/rest/v2/tables/delete)
#' @return
#' * `bq_table_copy()`, `bq_table_create()`, `bq_table_delete()`, `bq_table_upload()`:
#'   an invisible [bq_table]
#' * `bq_table_exists()`: either `TRUE` or `FALSE`.
#' * `bq_table_download()`: a data frame
#' * `bq_table_size()`: the size of the table in bytes
#' * `bq_table_fields()`: a [bq_fields].
#'
#' @examples
#' if (bq_testable()) {
#' ds <- bq_test_dataset()
#'
#' bq_mtcars <- bq_table_create(
#'   ds,
#'   "mtcars",
#'   friendly_name = "Motor Trend Car Road Tests",
#'   description = "The data was extracted from the 1974 Motor Trend US magazine",
#'   labels = list(category = "example")
#' )
#' bq_mtcars <- bq_table(ds, "mtcars")
#' bq_table_exists(bq_mtcars)
#'
#' bq_table_upload(bq_mtcars, mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' bq_table_fields(bq_mtcars)
#' bq_table_size(bq_mtcars)
#' str(bq_table_meta(bq_mtcars))
#'
#' bq_table_delete(bq_mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' my_natality <- bq_table(ds, "mynatality")
#' bq_table_copy("publicdata.samples.natality", my_natality)
#' }
#' @name api-table
NULL

#' @export
#' @rdname api-table
#' @param fields A [bq_fields] specification, or something coercible to it
#'   (like a data frame).
bq_table_create <- function(x, fields = NULL, ...) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, "")
  body <- list(
    tableReference = tableReference(x)
  )
  if (!is.null(fields)) {
    fields <- as_bq_fields(fields)
    body$schema <- list(fields = as_json(fields))
  }

  bq_post(url, body = bq_body(body, ...))

  x
}

#' @export
#' @rdname api-table
#' @inheritParams api-job
bq_table_meta <- function(x, fields = NULL) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  bq_get(url, query = list(fields = fields))
}

#' @export
#' @rdname api-table
bq_table_fields <- function(x) {
  meta <- bq_table_meta(x, fields = "schema")
  fields <- meta$schema$fields

  bq_fields(lapply(fields, as_bq_field))
}

#' @export
#' @rdname api-table
bq_table_size <- function(x) {
  meta <- bq_table_meta(x, fields = "numBytes")
  bytes <- as.numeric(meta$numBytes)
  structure(bytes, class = "bq_bytes")
}

#' @export
#' @rdname api-table
bq_table_nrow <- function(x) {
  meta <- bq_table_meta(x, fields = "numRows")
  as.numeric(meta$numRows)
}

#' @export
#' @rdname api-table
bq_table_exists <- function(x) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  bq_exists(url)
}

#' @export
#' @rdname api-table
bq_table_delete <- function(x) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  invisible(bq_delete(url))
}

#' @export
#' @rdname api-table
#' @inheritParams bq_perform_copy
#' @param dest Source and destination [bq_table]s.
bq_table_copy <- function(x, dest, ..., quiet = NA) {
  x <- as_bq_table(x)
  dest <- as_bq_table(dest)

  job <- bq_perform_copy(x, dest, ...)
  bq_job_wait(job, quiet = quiet)

  dest
}

#' @export
#' @rdname api-table
#' @inheritParams api-perform
bq_table_upload <- function(x, values, ..., quiet = NA) {
  x <- as_bq_table(x)

  job <- bq_perform_upload(x, values, ...)
  bq_job_wait(job, quiet = quiet)

  invisible(x)
}

#' @export
#' @rdname api-table
bq_table_save <- function(x, destination_uris, ..., quiet = NA) {
  x <- as_bq_table(x)

  job <- bq_perform_extract(x, destination_uris = destination_uris, ...)
  bq_job_wait(job, quiet = quiet)

  invisible(x)
}

#' @export
#' @rdname api-table
bq_table_load <- function(x, source_uris, ..., quiet = NA) {
  x <- as_bq_table(x)

  job <- bq_perform_load(x, source_uris = source_uris, ...)
  bq_job_wait(job, quiet = quiet)

  invisible(x)
}

#' @export
#' @rdname api-table
bq_table_patch <- function(x, fields) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, x$table)
  body <- list(
    tableReference = tableReference(x)
  )
  fields <- as_bq_fields(fields)
  body$schema <- list(fields = as_json(fields))
  bq_patch(url, body)
}
