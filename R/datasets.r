# https://developers.google.com/bigquery/docs/reference/v2/datasets

list_datasets <- function(project) {
  url <- sprintf("projects/%s/datasets", project)
  data <- bq_get(url)$datasets

  unlist(lapply(data, function(x) x$datasetReference$datasetId))
}
