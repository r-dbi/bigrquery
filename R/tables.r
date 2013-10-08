#' List available tables in dataset.
#'
#' @inheritParams insert_query_job
#' @return a character vector of table names
#' @family tables
#' @seealso API documentation:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/tables/list}
#' @export
#' @examples
#' \donttest{
#' list_tables("publicdata", "samples")
#' }
list_tables <- function(project, dataset) {
  assert_that(is.string(project), is.string(dataset))

  url <- sprintf("projects/%s/datasets/%s/tables", project, dataset)
  data <- bq_get(url)$tables
  do.call("rbind", lapply(data, as.data.frame, row.names = 1L))

  unlist(lapply(data, function(x) x$tableReference$tableId))
}

#' Retrieve table metadata
#'
#' @inheritParams insert_query_job
#' @param table name of the table
#' @seealso API documentation:
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/get}
#' @family tables
#' @return A table resource list, as described by
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables}
#' @export
#' @examples
#' \donttest{
#' get_table("publicdata", "samples", "natality")
#' }
get_table <- function(project, dataset, table) {
  assert_that(is.string(project), is.string(dataset), is.string(table))

  url <- sprintf("projects/%s/datasets/%s/tables/%s", project, dataset, table)
  bq_get(url)
}

#' Delete a table.
#'
#' @inheritParams insert_query_job
#' @param table name of the table
#' @seealso API documentation:
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/delete}
#' @family tables
#' @export
#' @examples
#' \donttest{
#' get_table("publicdata", "samples", "natality")
#' }
delete_table <- function(project, dataset, table) {
  assert_that(is.string(project), is.string(dataset), is.string(table))
  
  url <- sprintf("projects/%s/datasets/%s/tables/%s", project, dataset, table)
  bq_delete(url)
}

