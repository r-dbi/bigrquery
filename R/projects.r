# https://developers.google.com/bigquery/docs/reference/v2/projects

list_projects <- function() {
  data <- bq_get("projects")$projects

  id <- unlist(lapply(data, function(x) x$id))
  names(id) <- unlist(lapply(data, function(x) x$friendlyName))
  c(id, "sampledata" = "sampledata")
}
