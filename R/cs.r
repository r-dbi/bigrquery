#' Insert a job to load a CSV file from Google Cloud Storage into a table
#'
#' This is a low-level function that creates an insert job. To wait until it is
#' finished and retrive the new table, see [load_csv_from_cs()]
#'
#' @inheritParams insert_dataset
#' @param table The name of the table to load data into
#' @param source_uris The fully-qualified URIs that point to a CSV file in
#'   Google Cloud Storage; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.load.sourceUris}{the API documentation}
#'   for more information
#' @param autodetect boolean, default to `TRUE` to tell BigQuery to autodetect table schema
#' @param skip_leading_rows The number of rows at the top of a CSV file that
#'   BigQuery will skip when loading the data, default to 1
#' @param create_disposition Behavior for table creation if the destination
#'   already exists. defaults to \code{"CREATE_IF_NEEDED"},
#'   the only other supported value is \code{"CREATE_NEVER"}; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.load.createDisposition}{the API documentation}
#'   for more information
#' @param write_disposition Behavior for writing data if the destination already
#'   exists. defaults to \code{"WRITE_EMPTY"}, other possible values are
#'   \code{"WRITE_TRUNCATE"} and \code{"WRITE_APPEND"}; see
#'   \href{https://cloud.google.com/bigquery/docs/reference/v2/jobs#configuration.load.writeDisposition}{the API documentation}
#'   for more information
#' @family jobs
#' @return a job resource list, as documented at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs}
#' @seealso API documentation for insert method:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs/insert}
#' @export
insert_load_csv_from_cs_job <- function(project, dataset, table,
                                        source_uris,
                                        autodetect = TRUE,
                                        skip_leading_rows = 1,
                                        create_disposition = "CREATE_IF_NEEDED",
                                        write_disposition = "WRITE_EMPTY",
                                        ...) {
  assert_that(is.string(project), is.string(dataset), is.string(table))

  url <- bq_path(project, jobs = "")

  body <- list(
    configuration = list(
      load = list(
        sourceFormat = "CSV",
        sourceUris = source_uris,
        autodetect = autodetect,
        skipLeadingRows = skip_leading_rows,
        destinationTable = list(
          projectId = project,
          datasetId = dataset,
          tableId = table
        ),
        createDisposition = create_disposition,
        writeDisposition = write_disposition
      )
    )
  )

  bq_post(url, body = bq_body(body, ...))
}

#' Load a CSV file from Google Cloud Storage into a table
#'
#' Submit an insert job, waits for it, and invisibly returns information on the
#' destination table
#'
#' @inheritParams insert_load_csv_from_cs_job
#' @param quiet if `FALSE`, prints informative status messages.
#' @export
#' @examples
#' \dontrun{
#' project <- "fantastic-voyage-389" # put your project ID here
#' ds <- "test_dataset" # put your dataset name here
#' load_csv_from_cs(project = project, dataset = ds, table = "shakespeare",
#'                  source_uris = "gs://bigrquery_test/shakespeare.csv")
#' }
load_csv_from_cs <- function(project, dataset, table, source_uris, autodetect = TRUE,
                             skip_leading_rows = 1,
                             create_disposition = "CREATE_IF_NEEDED",
                             write_disposition = "WRITE_EMPTY",
                             quiet = getOption("bigrquery.quiet"),
                             ...) {
  job <- insert_load_csv_from_cs_job(project, dataset, table,
                                     source_uris = source_uris,
                                     autodetect = autodetect,
                                     skip_leading_rows = skip_leading_rows,
                                     create_disposition = create_disposition,
                                     write_disposition = write_disposition,
                                     ...)

  job <- wait_for(job, quiet = quiet)

  invisible(job$configuration$load$destinationTable)
}
