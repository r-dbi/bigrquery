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
