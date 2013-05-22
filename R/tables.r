

list_tables <- function(project, dataset) {
  url <- sprintf("projects/%s/datasets/%s/tables", project, dataset)
  data <- bq_get(url)$tables
  do.call("rbind", lapply(data, as.data.frame, row.names = 1L))

  unlist(lapply(data, function(x) x$tableReference$tableId))
}

get_table <- function(project, dataset, table) {
  url <- sprintf("projects/%s/datasets/%s/tables/%s", project, dataset, table)
  bq_get(url)
}

