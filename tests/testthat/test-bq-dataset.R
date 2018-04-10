context("test-bq-dataset.R")

test_that("can create and delete datasets", {
  ds <- bq_dataset(bq_test_project(), "dataset_api")
  expect_false(bq_dataset_exists(ds))

  bq_dataset_create(ds)
  expect_true(bq_dataset_exists(ds))

  bq_dataset_delete(ds)
  expect_false(bq_dataset_exists(ds))
})

test_that("by default can not delete dataset containing tables", {
  ds <- bq_test_dataset()

  bq_table_create(bq_table(ds, "testing"))
  expect_error(bq_dataset_delete(ds))
})

test_that("can list tables in a dataset", {
  ds <- bq_dataset(bq_test_project(), "basedata")

  expect_equal(
    bq_dataset_tables(ds),
    list(bq_table(ds, "mtcars"))
  )
})
