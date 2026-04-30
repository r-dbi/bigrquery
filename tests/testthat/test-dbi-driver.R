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

test_that("dbConnect() captures labels", {
  con <- dbConnect(
    bigquery(),
    project = bq_test_project(),
    labels = list(env = "test")
  )
  expect_equal(con@labels, list(env = "test"))
})

test_that("dbConnect() validates labels", {
  expect_snapshot(
    error = TRUE,
    dbConnect(bigquery(), project = bq_test_project(), labels = "oops")
  )
})

test_that("dbConnect() reads bigrquery.labels option", {
  withr::local_options(bigrquery.labels = list(env = "from-option"))
  con <- dbConnect(bigquery(), project = bq_test_project())
  expect_equal(con@labels, list(env = "from-option"))
})
