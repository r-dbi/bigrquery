context("test-dbi-connection.R")

test_that("can retrieve information about public dataset", {
  con <- DBI::dbConnect(
    bigquery(),
    project = "publicdata",
    dataset = "samples",
    billing = bq_test_project(),
    quiet = TRUE
  )

  expect_true("natality" %in% DBI::dbListTables(con))

  df <- DBI::dbReadTable(con, "natality", max_results = 10)
  expect_equal(nrow(df), 10)
})

test_that("can roundtrip a data frame", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(
    bigquery(),
    project = ds$project,
    dataset = ds$dataset,
    quiet = TRUE
  )
  DBI::dbWriteTable(con, "mtcars", mtcars)
  df <- DBI::dbReadTable(con, "mtcars")

  expect_equal(nrow(df), 32)
  expect_equal(ncol(df), 11)
})

test_that("dataset is optional", {
  con <- DBI::dbConnect(bigquery(), project = bq_test_project())
  expect_error(DBI::dbListTables(con), "`dataset`")

  df <- DBI::dbReadTable(con, "publicdata.samples.natality", max_results = 10)
  expect_equal(ncol(df), 31)

  expect_error(
    DBI::dbReadTable(con, "natality", max_results = 10),
    "must have 2 or 3 components"
  )
})
