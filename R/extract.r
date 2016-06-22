#' Create a new extract job.
#'
#' This is a low-level function that creates an extract job. To wait until it is
#' finished, see \code{\link{extract_exec}}
#'
#' @param source_table Source table,
#'   either as a string in the format used by BigQuery, or as a list with
#'   \code{project_id}, \code{dataset_id}, and \code{table_id} entries
#' @param project project name
#' @param destinationUris Specify the extract destination URI. Note: for large files, you may need to specify a wild-card since
#' @param compression Compression type ("NONE", "GZIP")
#' @param destinationFormat Destination format
#' @param fieldDelimiter Field delimiter (Default = ",")
#' @param printHeader Whether to print out a header row in the results. (Default is true)
#' @family jobs
#' @return a job resource list, as documented at
#'   \url{https://cloud.google.com/bigquery/docs/reference/v2/jobs}
#' @export
insert_extract_job <- function(source_table, project, destinationUris,
                               compression = "NONE", destinationFormat = "CSV",
                               fieldDelimiter = ",", printHeader = TRUE) {
  assert_that(is.string(project), is.string(source_table))

  if (!is.null(source_table)) {
    if (is.character(source_table)) {
      source_table <- parse_table(source_table, project_id = project)
    }
    assert_that(is.string(source_table$project_id),
                is.string(source_table$dataset_id),
                is.string(source_table$table_id))
  }

  url <- sprintf("projects/%s/jobs", project)
  body <- list(
    configuration = list(
      extract = list(
        sourceTable = list(
          datasetId = source_table$dataset_id,
          projectId = source_table$project_id,
          tableId = source_table$table_id
        ),
        destinationUris = destinationUris,
        destinationFormat = destinationFormat,
        compression = compression,
        fieldDelimiter = fieldDelimiter,
        printHeader = printHeader
      )
    )
  )

  bq_post(url, body)
}


#' Run a asynchronous extract job and wait till it is done
#'
#' This is a high-level function that inserts an extract job
#' (with \code{\link{insert_extract_job}}), repeatedly checks the status (with
#' \code{\link{get_job}}) until it is complete, then returns
#'
#' @inheritParams insert_extract_job
#' @seealso Google documentation for extracting data:
#'  \url{https://cloud.google.com/bigquery/exporting-data-from-bigquery}
#' @return The list of integer counts of files that were produced for each destinationUri
#' @export
#' @examples
#' \dontrun{
#' project <- "<my_project_id>" # specify your project ID here
#' bucket <- "gs://<my_bucket>/shakespeare*.csv" # specify your Cloud Storage bucket name (note the wildcard)
#'
#' # Now run the extract_exec - it will return the number of files that were extracted
#' extract_exec("publicdata:samples.shakespeare", project = project, destinationUris = bucket)
#' }
extract_exec <- function(source_table, project, destinationUris,
                               compression = "NONE", destinationFormat = "CSV",
                               fieldDelimiter = ",", printHeader = TRUE) {
  assert_that(is.string(source_table), is.string(project))

  job <- insert_extract_job(source_table, project, destinationUris,
                               compression, destinationFormat,
                               fieldDelimiter, printHeader)
  job <- wait_for(job)

  if(job$status$state == "DONE") {
    (job$statistics$extract$destinationUriFileCounts)
  } else {
    stop(paste0("Failed to extract ",source_table), call. = FALSE)
  }
}
