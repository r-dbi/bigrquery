#' Manipulate BigQuery tables
#'
#' @param x A [bq_table], or an object coercible to a `bq_table`.
#' @param src,dest Source and desintation [bq_table]s.
#' @param billing Project to bill. Defaults to the `dest` project.
#' @param create_disposition Behavior if the destination table does not already
#'   exist. Default will create table on demand; set to `"CREATE_NEVER"` to
#'   never create (and only copy into existing table).
#' @param write_disposition Behavior if the destination already exists.
#'   The default, `"WRITE_EMPTY"`, will error if the table already contains
#'   data. Other possible values are `"WRITE_TRUNCATE"` and `"WRITE_APPEND"`.
#' @section API documentation:
#' * [insert](https://developers.google.com/bigquery/docs/reference/v2/tables/insert)
#' * [get](https://developers.google.com/bigquery/docs/reference/v2/tables/get)
#' * [delete](https://developers.google.com/bigquery/docs/reference/v2/tables/delete)
#' @inheritParams wait_for
#' @return
#' * `bq_table_get()`: a [table resource list](https://developers.google.com/bigquery/docs/reference/v2/tables)
#' * `bq_table_get()`: either `TRUE` or `FALSE`
#' * `bq_table_copy()`: a table reference to the new table
#'
#' @examples
#' if (bq_testable()) {
#' ds <- bq_test_dataset()
#'
#' bq_mtcars <- bq_table(ds, "mtcars")
#' bq_table_exists(bq_mtcars)
#' str(bq_table_meta(bq_mtcars))
#'
#' bq_table_upload(bq_mtcars, mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' bq_table_delete(bq_mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' my_natality <- bq_table(ds, "mynatality")
#' bq_table_copy("publicdata.samples.natality", my_natality)
#' }
#' @name table-API
NULL

#' @export
#' @rdname table-API
#' @param A [bq_table]
#' @param ... Additional arguments passed on to the underlying BigQuery
#'   API function. `snake_case` arguments are automatically converted to
#'   `camelCase()`.
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
#' @rdname table-API
#' @inheritParams bq_job_meta
bq_table_meta <- function(x, fields = NULL) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  bq_get(url, query = list(fields = fields))
}

#' @export
#' @rdname table-API
bq_table_size <- function(x) {
  meta <- bq_table_meta(x, fields = "numBytes")
  bytes <- as.numeric(meta$numBytes)
  prettyunits::pretty_bytes(bytes)
}

#' @export
#' @rdname table-API
bq_table_exists <- function(x) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  bq_exists(url)
}

#' @export
#' @rdname table-API
bq_table_delete <- function(x) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  invisible(bq_delete(url))
}

#' @export
#' @rdname table-API
#' @inheritParams bq_perform_copy
bq_table_copy <- function(src, dest, ..., quiet = NA) {
  src <- as_bq_table(src)
  dest <- as_bq_table(dest)

  job <- bq_perform_copy(src, dest, ...)
  bq_job_wait(job, quiet = quiet)

  dest
}

#' @export
#' @rdname table-API
#' @inheritParams bq_perform_upload
bq_table_upload <- function(x, values, ..., quiet = NA) {
  x <- as_bq_table(x)

  job <- bq_perform_upload(x, ...)
  bq_job_wait(job, quiet = quiet)

  x
}

#' @export
#' @rdname table-API
bq_table_fields <- function(x) {
  meta <- bq_table_meta(x, fields = "schema")
  fields <- meta$schema$fields

  bq_fields(lapply(fields, as_bq_field))
}

#' @export
#' @rdname table-API
#' @inheritParams list_tabledata
bq_table_download <- function(x, billing = NULL, ...,
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
#' @rdname table-API
bq_table_extract <- function(x, urls, ..., quiet = NA) {
  x <- as_bq_table(x)

  job <- bq_perform_extract(x, destination_uris = urls, ...)
  bq_job_wait(job, quiet = quiet)

  x
}
