context("tables")



test_that("can create and list tables", {
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

test_that("table references are validated correctly", {
  valid <- list(project_id = "a", dataset_id = "b", table_id = "c")
  expect_that(validate_table_reference(valid), is_true())

  typo <- list(porject_id = "a", dataset_id = "b", table_id = "c")
  missing <- list(dataset_id = "b", table_id = "c")
  extra <- list(project_id = "a", dataset_id = "b", table_id = "c", job_id = "d")
  expect_that(validate_table_reference(typo), is_false())
  expect_that(validate_table_reference(missing), is_false())
  expect_that(validate_table_reference(extra), is_false())
})

test_that("table references can be merged", {
  minimal <- list(table_id = "c")
  partial <- list(dataset_id = "b", table_id = "c")
  complete <- list(project_id = "a", dataset_id = "b", table_id = "c")
  alternate <- list(project_id = "x", dataset_id = "y", table_id = "z")

  expect_that(merge_table_references(minimal, complete), equals(complete))
  expect_that(merge_table_references(partial, complete), equals(complete))
  expect_that(merge_table_references(alternate, complete), equals(alternate))
})


test_that("copy_table creates a copy of a table", {
  ds <- dataset_10_tables()
  src <- list(project_id = ds$project, dataset_id = ds$dataset, table_id = "table1")
  dest <- list(project_id = ds$project, dataset_id = ds$dataset, table_id = "table1_copy")

  copy_table(src, dest)
  res <- exists_table(ds$project, ds$dataset, "table1_copy")
})

test_that("copy_table validates arguments", {
    partial <- list(project_id = "a", dataset_id = "b")
    complete <- list(project_id = "x", dataset_id = "y", table_id = "z")

    expect_error(copy_table(list(), complete),
                 "src must be a table reference or a nonempty list of table references")
    expect_error(copy_table(complete, partial), "dest must be a table reference")
})

test_that("insert table creates table with a given schema", {
  project <- "bigrquery-examples"
  dataset <- "test_insert_tables"
  table <- "table_from_sample_schema"

  if (!exists_dataset(project, dataset)) {
    insert_dataset(project, dataset)
  }

  schema.source <- jsonlite::read_json("sample-schema.json")
  insert_table(project, dataset, table, schema.source)
  meta <- get_table(project, dataset, table)
  schema.meta <- meta$schema$fields
  expect_equal(length(schema.meta), 2, label = "Two fields were added per schema definition.")
  expect_equal(schema.meta, schema.source, label = "Table's schema matches definition.")
})

test_that("insert table creates table with time partitioning", {
  project <- "bigrquery-examples"
  dataset <- "test_insert_tables"
  table <- "table_with_time_partitioning"

  if (!exists_dataset(project, dataset)) {
    insert_dataset(project, dataset)
  }

  insert_table(project, dataset, table, partition = "DAY")
  meta <- get_table(project, dataset, table)
  partitioning <- meta$timePartitioning$type
  expect_equal(partitioning, "DAY", label = "Daily partitioning was added to the table.")
  expect_equal(schema.meta, schema.source)
})

teardown({
  project <- "bigrquery-examples"
  dataset <- "test_insert_tables"
  delete_dataset(project, dataset, deleteContents = TRUE)
})
