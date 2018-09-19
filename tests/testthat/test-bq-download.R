context("test-bq-download.R")

test_that("same results regardless of page size", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")

  df3 <- bq_table_download(tb, max_results = 30, page_size = 10)
  df1 <- bq_table_download(tb, max_results = 30, page_size = 30)
  expect_equal(nrow(df1), 30)
  expect_equal(df1, df3)
})

test_that("can retrieve fraction of page size", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, max_results = 15, page_size = 10)
  expect_equal(nrow(df), 15)
})

test_that("can retrieve zero rows", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, max_results = 0)
  expect_equal(nrow(df), 0)
  expect_named(df, c("phase", "phase_emoji", "peak_datetime"))
})

# types -------------------------------------------------------------------

test_that("can read utf-8 strings", {
  sql <- "SELECT '\U0001f603' as x"
  tb <- bq_project_query(bq_test_project(), sql)
  df <- bq_table_download(tb)
  x <- df$x[[1]]

  expect_equal(Encoding(x), "UTF-8")
  expect_equal(x, "\U0001f603")
})

test_that("can convert date time types", {
  sql <- "SELECT
    datetime,
    CAST (datetime as DATE) as date,
    CAST (datetime as TIME) as time,
    CAST (datetime as TIMESTAMP) as timestamp
    FROM (SELECT DATETIME '2000-01-02 03:04:05.67' as datetime)
  "

  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb)

  base <- ISOdatetime(2000, 1, 2, 3, 4, 5.67, tz = "UTC")

  expect_equal(df$datetime, base)
  expect_equal(df$timestamp, base)
  expect_equal(df$date, as.Date(base))
  expect_equal(df$time, hms::hms(hours = 3, minutes = 4, seconds = 5.67))
})

test_that("correctly parse logical values" ,{
  query <- "SELECT TRUE as x"
  tb <- bq_project_query(bq_test_project(), query)
  df <- bq_table_download(tb)

  expect_true(df$x)
})

test_that("the return type of integer columns is set by the bigint argument", {
  sql <- "
  SELECT *
  FROM UNNEST ([-2147483648, -2147483647, -1, 0, 1, 2147483647, 2147483648]) AS x;"

  default_col <-   bq_table_download( bq_project_query(bq_test_project(), sql) )$x
  integer64_col <- bq_table_download( bq_project_query(bq_test_project(), sql), bigint = "integer64" )$x
  integer_col <-   bq_table_download( bq_project_query(bq_test_project(), sql), bigint = "integer" )$x
  numeric_col <-   bq_table_download( bq_project_query(bq_test_project(), sql), bigint = "numeric" )$x
  character_col <- bq_table_download( bq_project_query(bq_test_project(), sql), bigint = "character" )$x

  integer64_target <- bit64::as.integer64( c("-2147483648", "-2147483647", "-1",
                                             "0", "1", "2147483647", "2147483648") )
  integer_target <- c(NA, -2147483647L, -1L, 0L, 1L, 2147483647L, NA)
  numeric_target <- c(-2147483648, -2147483647, -1, 0, 1, 2147483647, 2147483648)
  character_target <- c("-2147483648", "-2147483647", "-1",
                        "0", "1", "2147483647", "2147483648")

  expect_identical(default_col, integer64_target)
  expect_identical(integer64_col, integer64_target)
  expect_identical(integer_col, integer_target)
  expect_identical(numeric_col, numeric_target)
  expect_identical(character_col, character_target)
})
