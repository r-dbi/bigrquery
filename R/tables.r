#' Manipulate tables
#'
#' @param x A [bq_table]
#' @param src,dest Source and desintation [bq_table]s.
#' @param billing Project to bill. Defaults to the `dest` project.
#' @param create_disposition Behavior if the destination table does not already
#'   exist. Default will create table on demand; set to `"CREATE_NEVER"` to
#'   never create (and only copy into existing table).
#' @param write_disposition Behavior if the destination already exists.
#'   The default, `"WRITE_EMPTY"`, will error if the table already contains
#'   data. Other possible values are `"WRITE_TRUNCATE"` and `"WRITE_APPEND"`.
#' @section API documentation
#' * [create](https://developers.google.com/bigquery/docs/reference/v2/tables/insert)
#' * [get](https://developers.google.com/bigquery/docs/reference/v2/tables/get)
#' * [delete](https://developers.google.com/bigquery/docs/reference/v2/tables/delete)
#' * [copy](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs#configuration.copy)
#' @return
#' * `bq_table_get()`: a [table resource list](https://developers.google.com/bigquery/docs/reference/v2/tables)
#' * `bq_table_get()`: either `TRUE` or `FALSE`
#' * `bq_table_copy()`: a table reference to the new table
#'
#' @examples
#' \dontrun{
#' insert_dataset(bq_test_project(), "table_api")
#'
#' bq_mtcars <- bq_table(bq_test_project(), "table_api", "mtcars")
#' bq_table_exists(bq_mtcars)
#'
#' bq_table_upload(bq_mtcars, mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' bq_table_delete(bq_mtcars)
#' bq_table_exists(bq_mtcars)
#'
#' my_natality <- bq_table(bq_test_project(), "table_api", "mynatality")
#' bq_table_copy("publicdata:samples:natality", my_natality)
#'
#' delete_dataset(bq_test_project(), "table_api", deleteContents = TRUE)
#' }
#' @name table-API
NULL

#' @export
#' @rdname table-API
bq_table_create <- function(x, ...) {
  x <- as_bq_table(x)

  url <- bq_path(x$project, x$dataset, "")
  body <- list(
    tableReference = tableReference(x)
  )
  bq_post(url, body = bq_body(body, ...))
}

#' @export
#' @rdname table-API
bq_table_get <- function(x) {
  x <- as_bq_table(x)
  url <- bq_path(x$project, x$dataset, x$table)
  bq_get(url)
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
bq_table_copy <- function(src, dest,
                          create_disposition = "CREATE_IF_NEEDED",
                          write_disposition = "WRITE_EMPTY",
                          billing = NULL,
                          quiet = NA,
                          ...) {

  src <- as_bq_table(src)
  dest <- as_bq_table(dest)

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

  job <- bq_post(url, body = bq_body(body, ...))
  job <- wait_for(job, quiet = quiet)

  dest
}

#' @export
#' @rdname table-API
bq_table_upload <- function(x, values,
                            billing = NULL,
                            create_disposition = "CREATE_IF_NEEDED",
                            write_disposition = "WRITE_APPEND",
                            quiet = NA,
                            ...) {
  x <- as_bq_table(x)

  job <- insert_upload_job(x$project, x$dataset, x$table, values,
    billing = billing %||% x$project,
    create_disposition = create_disposition,
    write_disposition = write_disposition,
    ...
  )

  job <- wait_for(job, quiet = quiet)
  dest <- job$configuration$load$destinationTable

  bq_table(dest$projectId, dest$datasetId, dest$tableId)
}



# Older API ---------------------------------------------------------------

#' Deprecated table API
#'
#' See [table-API] for recommended API.
#' @keywords internal
#' @name table-dep
NULL

#' @rdname table-dep
#' @export
insert_table <- function(project, dataset, table, ...) {
  x <- bq_table(project, dataset, table)
  bq_table_insert(x, ...)
}

#' @rdname table-dep
#' @export
get_table <- function(project, dataset, table) {
  x <- bq_table(project, dataset, table)
  bq_table_get(x)
}

#' @rdname table-dep
#' @export
exists_table <- function(project, dataset, table) {
  x <- bq_table(project, dataset, table)
  bq_table_exists(x)
}

#' @rdname table-dep
#' @export
delete_table <- function(project, dataset, table) {
  x <- bq_table(project, dataset, table)
  bq_table_delete(x)
}
