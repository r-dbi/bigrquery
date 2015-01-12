#' List available tables in dataset.
#'
#' @inheritParams get_table
#' @param max_results (optional) Maximum number of results to
#'   retrieve.
#' @return a character vector of table names
#' @family tables
#' @seealso API documentation:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/tables/list}
#' @export
#' @examples
#' \dontrun{
#' list_tables("publicdata", "samples")
#' list_tables("githubarchive", "github")
#' list_tables("publicdata", "samples", max_results = 2)
#' }
list_tables <- function(project, dataset, max_results = NULL) {
  assert_that(is.string(project), is.string(dataset))
  if (!is.null(max_results)) {
    assert_that(is.numeric(max_results), length(max_results) == 1)
  }

  url <- sprintf("projects/%s/datasets/%s/tables", project, dataset)
  query <- list()
  if (!is.null(max_results)) {
    query$maxResults <- max_results
  }
  data <- bq_get(url, query = query)$tables
  do.call("rbind", lapply(data, as.data.frame, row.names = 1L))

  unlist(lapply(data, function(x) x$tableReference$tableId))
}

#' Retrieve table metadata
#'
#' @param project project containing this table
#' @param dataset dataset containing this table
#' @param table name of the table
#' @seealso API documentation:
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/get}
#' @family tables
#' @return A table resource list, as described by
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables}
#' @export
#' @examples
#' \dontrun{
#' get_table("publicdata", "samples", "natality")
#' get_table("githubarchive", "github", "timeline")
#' }
get_table <- function(project, dataset, table) {
  assert_that(is.string(project), is.string(dataset), is.string(table))

  url <- sprintf("projects/%s/datasets/%s/tables/%s", project, dataset, table)
  bq_get(url)
}

#' Delete a table.
#'
#' @inheritParams get_table
#' @seealso API documentation:
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/delete}
#' @family tables
#' @export
#' @examples
#' \dontrun{
#' get_table("publicdata", "samples", "natality")
#' }
delete_table <- function(project, dataset, table) {
  assert_that(is.string(project), is.string(dataset), is.string(table))

  url <- sprintf("projects/%s/datasets/%s/tables/%s", project, dataset, table)
  bq_delete(url)
}

