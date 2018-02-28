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

test_that("copy_table validates its arguments", {
  partial <- list(project_id = "a", dataset_id = "b")
  complete <- list(project_id = "x", dataset_id = "y", table_id = "z")

  expect_error(copy_table(list(), complete),
               "src must be a table reference or a nonempty list of table references")
  expect_error(copy_table(complete, partial), "dest must be a table reference")
})
