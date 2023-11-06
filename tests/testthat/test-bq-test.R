test_that("can init and clean up dataset", {
  ds <- bq_test_dataset()
  expect_true(bq_dataset_exists(ds))

  bq_test_init(ds$dataset)
  expect_true(bq_table_exists(bq_table(ds, "mtcars")))

  attr(ds, "env") <- NULL
  gc()
  expect_false(bq_dataset_exists(ds))
})

test_that("error if env var not set", {
  withr::local_envvar(c(
    BIGQUERY_TEST_PROJECT = "",
    BIGQUERY_TEST_BUCKET = "",
    TESTTHAT = ""
  ))

  expect_snapshot(error = TRUE, {
    bq_test_project()
    gs_test_bucket()
  })
})
