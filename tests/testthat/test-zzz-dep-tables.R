context("dep-tables.R")

dataset_10_tables <- function() {
  skip_if_no_auth()

  project <- bq_test_project()
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

test_that("can list tables", {
  ds <- dataset_10_tables()
  expect_equal(
    list_tables(ds$project, ds$dataset),
    sort(paste0("table", 1:10))
  )
})

test_that("can control pagination of list_tables", {
  tables <- sort(paste0("table", 1:10))
  ds <- dataset_10_tables()

  expect_equal(list_tables(ds$project, ds$dataset, page_size = 5), tables)
  expect_equal(list_tables(ds$project, ds$dataset, page_size = 10), tables)

  expect_equal(
    list_tables(ds$project, ds$dataset, page_size = 1, max_pages = 1),
    tables[1]
  )
  expect_equal(
    list_tables(ds$project, ds$dataset, page_size = 1, max_pages = 2),
    tables[1:2]
  )
})
