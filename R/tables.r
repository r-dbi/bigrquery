#' List available tables in dataset.
#'
#' @inheritParams get_table
#' @param page_size Number of items per page
#' @param max_pages Maximum number of pages to retrieve
#' @return a character vector of table names
#' @family tables
#' @seealso API documentation:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/tables/list}
#' @export
#' @examples
#' \dontrun{
#' list_tables("publicdata", "samples")
#' list_tables("githubarchive", "github")
#' list_tables("publicdata", "samples", max_pages = 2, page_size = 2)
#' }
list_tables <- function(project, dataset, page_size = 50, max_pages = Inf) {
  data <- bq_get_paginated(
    bq_path(project, dataset, ""),
    page_size = page_size,
    max_pages = max_pages
  )

  tables <- unlist(lapply(data, function(x) x$tables), recursive = FALSE)
  vapply(tables, function(x) x$tableReference$tableId, character(1L))
}

#' Insert empty table
#'
#' @inheritParams insert_dataset
#' @inheritParams get_table
#' @export
#' @seealso API documentation:
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/insert}
insert_table <- function(project, dataset, table, ...) {
  url <- bq_path(project, dataset, "")
  body <- list(
    tableReference = list(
      projectId = project,
      datasetId = dataset,
      tableId = table
    )
  )

  bq_post(url, body = bq_body(body, ...))
}

#' Retrieve table metadata
#'
#' @inheritParams insert_dataset
#' @param table name of the table
#' @seealso API documentation:
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/get}
#' @family tables
#' @return A table resource list, as described by
#'  \url{https://developers.google.com/bigquery/docs/reference/v2/tables}
#' @export
#' @examples
#' \dontrun{
#' str(get_table("publicdata", "samples", "natality"))
#' str(get_table("publicdata", "samples", "gsod"))
#' str(get_table("githubarchive", "github", "timeline"))
#' }
#'
#' @description `get_table` returns a table's metadata as a nested list.
#'   In addition to a regular error, the condition `bigrquery_notFound`
#'   (which can be handled via [base::tryCatch()])
#'   is raised if the table could not be found.
get_table <- function(project, dataset, table) {
  assert_that(is.string(project), is.string(dataset), is.string(table))

  url <- sprintf("projects/%s/datasets/%s/tables/%s", project, dataset, table)
  bq_get(url)
}

#' @rdname get_table
#' @export
#' @description `exists_table` merely checks if a table exists, and returns
#'   either `TRUE` or `FALSE`.
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
  bq_delete(bq_path(project, dataset, table))
}

validate_table_reference <- function(reference) {
  required_keys <- c('project_id', 'dataset_id', 'table_id')
  is.list(reference) && setequal(required_keys, names(reference))
}

as_bigquery_table_reference <- function(reference) {
  list(
    projectId = reference$project_id,
    datasetId = reference$dataset_id,
    tableId = reference$table_id
  )
}

merge_table_references <- function(partial, complete) {
  list(
    project_id = partial$project_id %||% complete$project_id,
    dataset_id = partial$dataset_id %||% complete$dataset_id,
    table_id = partial$table_id
  )
}

#' Copy one or more source tables to a destination table.
#'
#' Each source table and the destination table should be table references, that
#' is, lists with exactly three entries: `project_id`, `dataset_id`,
#' and `table_id`.
#'
#' @param src either a single table reference, or a list of table references
#' @param dest destination table
#' @param project project ID to use for the copy job. defaults to the project of
#'   the destination table.
#' @param create_disposition behavior for table creation if the destination
#'   already exists. defaults to `"CREATE_IF_NEEDED"`,
#'   the only other supported value is `"CREATE_NEVER"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.copy.createDisposition}{the API documentation}
#'   for more information
#' @param write_disposition behavior for writing data if the destination already
#'   exists. defaults to `"WRITE_EMPTY"`, other possible values are
#'   `"WRITE_TRUNCATE"` and `"WRITE_APPEND"`; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.copy.writeDisposition}{the API documentation}
#'   for more information
#' @inheritParams insert_dataset
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
                       project = NULL,
                       ...) {
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
  url <- bq_path(project, jobs = "")
  body <- list(
    projectId = project,
    configuration = list(
      copy = list(
        sourceTables = lapply(src, as_bigquery_table_reference),
        destinationTable = as_bigquery_table_reference(dest),
        createDisposition = create_disposition,
        writeDisposition = write_disposition
      )
    )
  )

  bq_post(url, body = bq_body(...))
}
