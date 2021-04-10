#' Table API [deprecated]
#'
#' See [api-table] for recommended API.
#' @keywords internal
#' @name table-dep
NULL

#' @rdname table-dep
#' @export
insert_table <- function(project, dataset, table, ...) {
  .Deprecated("bq_table_upload", package = "bigrquery")
  x <- bq_table(project, dataset, table)
  bq_table_create(x, ...)
}

#' @rdname table-dep
#' @export
get_table <- function(project, dataset, table) {
  .Deprecated("bq_table_meta", package = "bigrquery")
  x <- bq_table(project, dataset, table)
  bq_table_meta(x)
}

#' @rdname table-dep
#' @export
exists_table <- function(project, dataset, table) {
  .Deprecated("bq_table_exists", package = "bigrquery")
  x <- bq_table(project, dataset, table)
  bq_table_exists(x)
}

#' @rdname table-dep
#' @export
delete_table <- function(project, dataset, table) {
  .Deprecated("bq_table_delete", package = "bigrquery")
  x <- bq_table(project, dataset, table)
  bq_table_delete(x)
}
