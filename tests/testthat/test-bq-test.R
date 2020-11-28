test_that("can init new dataset", {
  ds <- bq_test_dataset()
  bq_test_init(ds$dataset)
  expect_true(bq_dataset_exists(ds))
})

test_that("error if env var not set", {
  withr::local_envvar(c(
    BIGQUERY_TEST_PROJECT = "",
    BIGQUERY_TEST_BUCKET = "",
    TESTTHAT = ""
  ))

  expect_error(bq_test_project(), "BIGQUERY_TEST_PROJECT")
  expect_error(gs_test_bucket(), "BIGQUERY_TEST_BUCKET")
})
