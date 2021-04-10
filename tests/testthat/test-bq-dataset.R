test_that("can create and delete datasets", {
  ds <- bq_dataset(bq_test_project(), random_name())
  expect_false(bq_dataset_exists(ds))

  bq_dataset_create(ds)
  expect_true(bq_dataset_exists(ds))

  bq_dataset_delete(ds)
  expect_false(bq_dataset_exists(ds))
})

test_that("can update dataset metadata", {
  ds <- bq_dataset(bq_test_project(), random_name())
  on.exit(bq_dataset_delete(ds))

  bq_dataset_create(ds, description = "a", friendly_name = "b")
  bq_dataset_update(ds, description = "b")

  meta <- bq_dataset_meta(ds, "description,friendlyName")
  expect_equal(meta$description, "b")
  expect_equal(meta$friendlyName, "b")
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
