context("query_exec")

test_that("can convert all date time types", {
  skip_if_no_auth()

  sql <- "SELECT
    datetime,
    CAST (datetime as DATE) as date,
    CAST (datetime as TIME) as time,
    CAST (datetime as TIMESTAMP) as timestamp
    FROM (SELECT DATETIME '2000-01-02 03:04:05.67' as datetime)
  "

  df <- query_exec(sql, project = "bigrquery-examples", use_legacy_sql = FALSE)

  base <- ISOdatetime(2000, 1, 2, 3, 4, 5.67, tz = "UTC")
  expect_equal(df$datetime, base)
  expect_equal(df$timestamp, base)

  expect_equal(df$date, as.Date(base))
  expect_equal(df$time, hms::hms(hours = 3, minutes = 4, seconds = 5.67))
})
