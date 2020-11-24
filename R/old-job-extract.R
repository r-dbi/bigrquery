#' Create a new extract job [deprecated]
#'
#' Please use [api-job] and [api-perform] instead.
#'
#' @keywords internal
#' @inheritParams insert_upload_job
#' @param destination_uris Fully qualified google storage url. For large
#'    extracts you may need to specify a wild-card since
#' @param destination_format Destination format ("CSV", "ARVO", or
#'   "NEWLINE_DELIMITED_JSON")
#' @param compression Compression type ("NONE", "GZIP")
#' @param print_header Include row of column headers in the results?
#' @family jobs
#' @return a job resource list, as documented at
#'   \url{https://cloud.google.com/bigquery/docs/reference/rest/v2/jobs}
#' @export
insert_extract_job <- function(project, dataset, table,
                               destination_uris,
                               compression = "NONE",
                               destination_format = "NEWLINE_DELIMITED_JSON",
                               ...,
                               print_header = TRUE,
                               billing = project) {

  .Deprecated("bq_perform_load", package = "bigrquery")

  assert_that(
    is.string(project),
    is.string(dataset),
    is.string(table),
    is.character(destination_uris),
    is.string(billing)
  )

  url <- bq_path(billing, jobs = "")
  body <- list(
    configuration = list(
      extract = list(
        sourceTable = list(
          projectId = project,
          datasetId = dataset,
          tableId = table
        ),
        destinationUris = destination_uris,
        destinationFormat = destination_format,
        compression = compression,
        printHeader = print_header
      )
    )
  )

  bq_post(url, body = bq_body(body, ...))
}
