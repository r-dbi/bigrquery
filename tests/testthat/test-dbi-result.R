context("test-dbi-result.R")

test_that("can retrieve full query results", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata",
    billing = bq_test_project(),
    quiet = TRUE
  )

  df <- DBI::dbGetQuery(con, "SELECT count(*) as count FROM mtcars")
  expect_equal(df, data.frame(count = 32))
})

test_that("can retrieve query in pieces", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata",
    billing = bq_test_project(),
    quiet = TRUE
  )

  res <- DBI::dbSendQuery(con, "SELECT cyl, mpg FROM mtcars")
  df <- DBI::dbFetch(res, 10)
  expect_equal(nrow(df), 10)
  expect_false(DBI::dbHasCompleted(res))

  df <- DBI::dbFetch(res, -1)
  expect_equal(nrow(df), 22)
  expect_true(DBI::dbHasCompleted(res))
})
