test_that("can retrieve full query results", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata",
    bigint = "integer"
  )

  df <- DBI::dbGetQuery(con, "SELECT count(*) as count FROM mtcars")
  expect_equal(df, tibble(count = 32L))
})

test_that("can retrieve without dataset", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    bigint = "integer"
  )
  df <- DBI::dbGetQuery(con, "SELECT count(*) as count FROM `basedata.mtcars`")
  expect_equal(df, tibble(count = 32L))
})

test_that("can retrieve query in pieces", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  res <- DBI::dbSendQuery(con, "SELECT cyl, mpg FROM mtcars")
  expect_equal(DBI::dbGetRowCount(res), 0L)

  df <- DBI::dbFetch(res, 10)
  expect_equal(nrow(df), 10)
  expect_false(DBI::dbHasCompleted(res))
  expect_equal(DBI::dbGetRowCount(res), 10L)

  df <- DBI::dbFetch(res, -1)
  expect_equal(nrow(df), 22)
  expect_true(DBI::dbHasCompleted(res))
})

test_that("can get metadata", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )
  sql <- "SELECT cyl, mpg FROM mtcars"
  res <- DBI::dbSendQuery(con, sql)
  expect_known_output(print(res), "dbi-result-print.txt")

  col_info <- DBI::dbColumnInfo(res)
  expect_equal(dim(col_info), c(2, 2))
  expect_equal(col_info$name, c("cyl", "mpg"))

  expect_equal(DBI::dbGetStatement(res), sql)

})

test_that("dbExecute returns modified rows", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(ds)

  DBI::dbExecute(con, "CREATE TABLE foo (a INT64)")
  expect_equal(DBI::dbExecute(con, "INSERT INTO foo VALUES (1), (2), (3)"), 3)
  expect_equal(DBI::dbExecute(con, "DELETE FROM foo WHERE a >= 2"), 2)
  DBI::dbExecute(con, "DROP TABLE foo")
})
