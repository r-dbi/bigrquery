context("dataset.R")

test_that("can create and delete datasets", {
  ds <- bq_dataset(bq_test_project(), "dataset_api")
  expect_false(bq_dataset_exists(ds))

  bq_dataset_create(ds)
  expect_true(bq_dataset_exists(ds))

  bq_dataset_delete(ds)
  expect_false(bq_dataset_exists(ds))
})

test_that("can list tables in a dataset", {
  ds <- bq_test_dataset()

  x1 <- bq_table_create(bq_table(ds, "x1"))
  x2 <- bq_table_create(bq_table(ds, "x2"))

  expect_equal(bq_dataset_tables(ds), list(x1, x2))
})

# Old api -----------------------------------------------------------------

test_that("extra arguments passed onto request", {
  insert_dataset(bq_test_project(), "test_temp",
    friendlyName = "xxx",
    description = "yyy"
  )
  on.exit(delete_dataset(bq_test_project(), "test_temp"))

  ds <- get_dataset(bq_test_project(), "test_temp")
  expect_equal(ds$friendlyName, "xxx")
  expect_equal(ds$description, "yyy")
})
