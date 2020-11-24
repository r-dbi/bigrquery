test_that("driver is always valid", {
  expect_true(DBI::dbIsValid(bigquery()))
})

test_that("dbi_driver is deprecated", {
  expect_warning(dbi_driver(), "deprecated")
})

test_that("connecting yields a BigQueryConnection", {
  con <- dbConnect(bigquery(), project = bq_test_project())
  expect_s4_class(con, "BigQueryConnection")
})
