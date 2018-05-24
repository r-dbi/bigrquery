context("dataset.R")

test_that("extra arguments passed onto request", {
  name <- random_name()

  insert_dataset(bq_test_project(), name,
    friendlyName = "xxx",
    description = "yyy"
  )
  on.exit(delete_dataset(bq_test_project(), name))

  ds <- get_dataset(bq_test_project(), name)
  expect_equal(ds$friendlyName, "xxx")
  expect_equal(ds$description, "yyy")
})
