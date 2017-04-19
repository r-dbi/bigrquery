dataset_10_tables <- function() {
  skip_if_no_auth()

  project <- "bigrquery-examples"
  dataset <- "test_10_tables"

  if (!exists_dataset(project, dataset)) {
    insert_dataset(project, dataset)
    tables <- paste0("table", 1:10)
    for (table in tables) {
      insert_table(project, dataset, table)
    }
  }

  list(
    project = project,
    dataset = dataset
  )
}
