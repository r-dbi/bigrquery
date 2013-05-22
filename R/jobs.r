# https://developers.google.com/bigquery/docs/reference/v2/jobs

#' Create a new query job.
#'
#' This is a low-level function that creates a query job. To wait until it
#' is finished and then retrieve the results, see \code{\link{query_exec}}
#'
#' @param project project name
#' @param dataset dataset name
#' @param query SQL query string
#' @param billing project to bill to, if different to \code{project}
#' @family jobs
#' @return a job resource list, as documented at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs}
#' @seealso API documentation for insert method:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs/insert}
#' @export
insert_query_job <- function(project, dataset, query, billing = project) {
  assert_that(is.string(project), is.string(dataset), is.string(query),
    is.string(billing))

  url <- sprintf("projects/%s/jobs", billing)
  body <- list(
    kind = "bigquery#job",
    configuration = list(
      query = list(
        query = query,
        defaultDataset = list(
          projectId = project,
          datasetId = dataset
        )
      )
    )
  )
  bq_post(url, body)
}

#' Check status of a job.
#'
#' @param project project name
#' @param job job id
#' @return a job resource list, as documented at
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs}
#' @seealso API documentation for get method:
#'   \url{https://developers.google.com/bigquery/docs/reference/v2/jobs/get}
#' @export
get_job <- function(project, job) {
  assert_that(is.string(project), is.string(job))

  url <- sprintf("projects/%s/jobs/%s", project, job)
  bq_get(url)
}
