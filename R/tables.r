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
#'
#' @description \code{get_table} returns a table's metadata as a nested list.
#'   In addition to a regular error, the condition \code{bigrquery_notFound}
#'   (which can be handled via \code{\link[base]{tryCatch}})
#'   is raised if the table could not be found.
get_table <- function(project, dataset, table) {
  assert_that(is.string(project), is.string(dataset), is.string(table))

  url <- sprintf("projects/%s/datasets/%s/tables/%s", project, dataset, table)
  bq_get(url)
}

#' @rdname get_table
#' @export
#' @description \code{exists_table} merely checks if a table exists, and returns
#'   either \code{TRUE} or \code{FALSE}.
exists_table <- function(project, dataset, table) {
  tryCatch(
    !is.null(get_table(project = project, dataset = dataset, table = table)),
    bigrquery_notFound = function(e) FALSE
  )
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

validate_table_reference <- function(reference) {
  required_keys <- c('project_id', 'dataset_id', 'table_id')
  is.list(reference) && setequal(required_keys, names(reference))
}

as_bigquery_table_reference <- function(reference) {
  list(projectId = reference$project_id,
       datasetId = reference$dataset_id,
       tableId = reference$table_id)
}

merge_table_references <- function(partial, complete) {
  list(project_id = partial$project_id %||% complete$project_id,
       dataset_id = partial$dataset_id %||% complete$dataset_id,
       table_id = partial$table_id)
}

#' Copy one or more source tables to a destination table.
#'
#' Each source table and the destination table should be table references, that
#' is, lists with exactly three entries: \code{project_id}, \code{dataset_id},
#' and \code{table_id}.
#'
#' @param src either a single table reference, or a list of table references
#' @param dest destination table
#' @param project project ID to use for the copy job. defaults to the project of
#'   the destination table.
#' @param create_disposition behavior for table creation if the destination
#'   already exists. defaults to \code{"CREATE_IF_NEEDED"},
#'   the only other supported value is \code{"CREATE_NEVER"}; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.copy.createDisposition}{the API documentation}
#'   for more information
#' @param write_disposition behavior for writing data if the destination already
#'   exists. defaults to \code{"WRITE_EMPTY"}, other possible values are
#'   \code{"WRITE_TRUNCATE"} and \code{"WRITE_APPEND"}; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.copy.writeDisposition}{the API documentation}
#'   for more information
#' @seealso API documentation:
#'   \url{https://cloud.google.com/bigquery/docs/tables#copyingtable}
#' @export
#' @examples
#' \dontrun{
#' src <- list(project_id = "publicdata", dataset_id = "samples", table_id = "shakespeare")
#' dest <- list(project_id = "myproject", dataset_id = "mydata", table_id = "shakespeare")
#' doubled <- dest
#' doubled$table_id <- "double_shakespeare"
#' copy_table(src, dest)
#' copy_table(list(src, dest), doubled)
#' }
copy_table <- function(src, dest,
                       create_disposition = "CREATE_IF_NEEDED",
                       write_disposition = "WRITE_EMPTY",
                       project = NULL) {
  if (validate_table_reference(src)) {
    src <- list(src)
  } else if (!all(vapply(src, validate_table_reference, TRUE)) ||
             (length(src) == 0)) {
    stop("src must be a table reference or a nonempty list of table references")
  }
  if (!validate_table_reference(dest)) {
    stop("dest must be a table reference")
  }
  project <- project %||% dest$project_id
  url <- sprintf("projects/%s/jobs", project)
  body <- list(
      projectId = project,
      configuration = list(
          copy = list(
              sourceTables = lapply(src, as_bigquery_table_reference),
              destinationTable = as_bigquery_table_reference(dest),
              createDisposition = create_disposition,
              writeDisposition = write_disposition
          )))

  bq_post(url, body)
}
