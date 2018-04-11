#' BigQuery tables
#'
#' Basic create-read-update-delete verbs for tables, as well as functions
#' for uploading and downloading table data.
#'
#' @param x A [bq_table], or an object coercible to a `bq_table`.
#' @inheritParams api-job
#' @inheritParams api-perform
#' @inheritParams bq_projects
#' @section API documentation:
#' * [insert](https://developers.google.com/bigquery/docs/reference/v2/tables/insert)
#' * [get](https://developers.google.com/bigquery/docs/reference/v2/tables/get)
#' * [delete](https://developers.google.com/bigquery/docs/reference/v2/tables/delete)
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
bq_table_create <- function(x, ...) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, "")
  body <- list(
    tableReference = tableReference(x)
  )
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
#' @param dest Source and desintation [bq_table]s.
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
#' @inheritParams list_tabledata
#' @inheritParams bq_projects
bq_table_download <- function(x,
                              billing = NULL,
                              ...,
                              page_size = 1e4,
                              max_pages = 10,
                              warn = TRUE,
                              quiet = NA) {

  x <- as_bq_table(x)
  assert_that(is.numeric(max_pages), length(max_pages) == 1)

  table_info <- bq_table_meta(x)
  if (max_pages < 1) {
    rows <- extract_data(NULL, table_info$schema)
    return(rows)
  }

  # This is a rather inefficient implementation - better strategy would be
  # preallocate list when max_pages is finite, and use doubling strategy
  # when it's not.
  rows <- list()
  append_rows <- function(new_rows) {
    rows <<- c(rows, list(new_rows))
  }

  list_tabledata_callback(
    x$project,
    x$dataset,
    x$table,
    append_rows,
    table_info = table_info,
    page_size = page_size,
    max_pages = max_pages,
    warn = warn,
    quiet = quiet
  )

  do.call("rbind", rows)
}

#' @export
#' @rdname api-table
bq_table_extract <- function(x, destination_uris, ..., quiet = NA) {
  x <- as_bq_table(x)

  job <- bq_perform_extract(x, destination_uris = destination_uris, ...)
  bq_job_wait(job, quiet = quiet)

  x
}
