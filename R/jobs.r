# https://developers.google.com/bigquery/docs/reference/v2/jobs

insert_job <- function(project, dataset, query, billing = project) {
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

get_job <- function(project, job) {
  url <- sprintf("projects/%s/jobs/%s", project, job)
  bq_get(url)
}

get_query_results_job <- function(job, project) {
  url <- sprintf("projects/%s/queries/%s", project, job)
  bq_get(url)
}
