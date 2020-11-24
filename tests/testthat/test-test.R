test_that("bq_test_dataset automatically cleans up", {
  ds <- bq_test_dataset()
  expect_true(bq_dataset_exists(ds))

  attr(ds, "env") <- NULL
  gc()

  expect_false(bq_dataset_exists(ds))
})
