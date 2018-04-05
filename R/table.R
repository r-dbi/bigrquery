#' Reference to a BigQuery table
#'
#' `bq_table()` creates a reference from individual `project`, `dataset`, and
#' `table` identifiers; `as_bq_table()` will create from a character vector or
#' a list. See [table-API] for operations that work with the remote table.
#'
#' @export
#' @param project,dataset,table Path of identifier to a table.
#'   If `project` is a [bq_dataset], then `dataset` will be taken
#'   as the table name.
#' @param x An object to coerce to a `bq_table`.
#' @examples
#' natality <- bq_table("publicdata", "samples", "natality")
#' natality
#'
#' # If you already have a dataset
#' ds <- bq_dataset("publicdata", "samples")
#' bq_table(ds, "natality")
#'
#' as_bq_table("publicdata.samples.natality")
#'
#' # Included primarily for backward compatbility
#' as_bq_table(list(
#'   project_id = "publicdata",
#'   dataset_id = "samples",
#'   table_id = "natality"
#' ))
bq_table <- function(project, dataset, table = NULL) {
  if (inherits(project, "bq_dataset") && is.null(table)) {
    return(bq_table(project$project, project$dataset, dataset))
  }

  assert_that(is.string(project), is.string(dataset), is.string(table))

  structure(
    list(
      project = project,
      dataset = dataset,
      table = table
    ),
    class = "bq_table"
  )
}

#' @export
print.bq_table <- function(x, ...) {
  cat_line("<bq_table> ", x$project, ".", x$dataset, ".", x$table)
}

# Coercion ---------------------------------------------------------------

#' @export
#' @rdname bq_table
as_bq_table <- function(x) UseMethod("as_bq_table")

#' @export
as_bq_table.bq_table <- function(x) {
  x
}

#' @export
as_bq_table.character <- function(x) {
  assert_that(is.string(x))
  pieces <- strsplit(x, ".", fixed = TRUE)[[1]]

  if (length(pieces) != 3) {
    stop(
      "Character <bq_table> must have exactly three components when split by .",
      call. = FALSE)
  }
  bq_table(pieces[[1]], pieces[[2]], pieces[[3]])
}

#' @export
as_bq_table.list <- function(x) {
  if (!setequal(names(x), c("project_id", "dataset_id", "table_id"))) {
    stop(
      "List <bq_table> must have components 'project_id', 'dataset_id', and 'table_id'",
      call. = FALSE
    )
  }
  bq_table(x$project_id, x$dataset_id, x$table_id)
}

# API ---------------------------------------------------------------------

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
#' * [create](https://developers.google.com/bigquery/docs/reference/v2/tables/insert)
#' * [get](https://developers.google.com/bigquery/docs/reference/v2/tables/get)
#' * [delete](https://developers.google.com/bigquery/docs/reference/v2/tables/delete)
#' * [copy](https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs#configuration.copy)
#' @inheritParams wait_for
#' @inheritParams bq_datasets
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
bq_table_meta <- function(x) {
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

tableReference <- function(x) {
  list(
    projectId = unbox(x$project),
    datasetId = unbox(x$dataset),
    tableId = unbox(x$table)
  )
}
