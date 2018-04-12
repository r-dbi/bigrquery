context("dataset.R")

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
