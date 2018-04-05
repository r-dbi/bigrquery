context("query_exec.R")

test_that("can convert all date time types", {
  proj <- bq_test_project()

  sql <- "SELECT
    datetime,
    CAST (datetime as DATE) as date,
    CAST (datetime as TIME) as time,
    CAST (datetime as TIMESTAMP) as timestamp
    FROM (SELECT DATETIME '2000-01-02 03:04:05.67' as datetime)
  "

  df <- query_exec(sql, project = proj, use_legacy_sql = FALSE)

  base <- ISOdatetime(2000, 1, 2, 3, 4, 5.67, tz = "UTC")
  expect_equal(df$datetime, base)
  expect_equal(df$timestamp, base)

  expect_equal(df$date, as.Date(base))
  expect_equal(df$time, hms::hms(hours = 3, minutes = 4, seconds = 5.67))
})

test_that("data is not returned if max_pages is set to zero", {
  proj <- bq_test_project()

  sql <- "SELECT 1 x, 2 y"
  res <- query_exec(sql, proj, max_pages = 0)
  expect_equal(res, data.frame(x = numeric(), y = numeric()))
})

test_that("query with no results returns empty dataset with field names", {
  proj <- bq_test_project()

  sql <- "SELECT * FROM (SELECT 1 AS test) WHERE FALSE"
  df <- query_exec(sql, proj, use_legacy_sql = FALSE)
  expect_identical(nrow(df), 0L)
  expect_identical(colnames(df), "test")
})
