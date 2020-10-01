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

test_that("can specify large integers in page params", {
  skip_if_no_auth()

  # Use scipen to nudge R to prefer scientific formatting for very small numbers
  # to allow this test to exercise issue #395 with small datasets.
  old <- options(scipen=-4)
  on.exit(options(old))

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, max_results = 100, page_size = 20)
  expect_equal(nrow(df), 100)
})

# bq_table_info -----------------------------------------------------------

test_that("max_results + start_index affects end values", {
  out <- bq_download_page_info(
    nrow = 100,
    max_results = 5,
    page_size = 2,
    start_index = 5
  )
  expect_equal(out$begin, c(5, 7, 9))
  expect_equal(out$end, c(7, 9, 10))
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
  x <- c("-2147483648", "-2147483647", "-1", "0", "1", "2147483647", "2147483648")
  sql <- paste0("SELECT * FROM UNNEST ([", paste0(x, collapse = ","), "]) AS x");
  qry <- bq_project_query(bq_test_project(), sql)

  expect_warning(
    out_int <- bq_table_download(qry, bigint = "integer")$x,
    "integer overflow"
  )
  expect_identical(out_int, suppressWarnings(as.integer(x)))

  out_int64 <- bq_table_download(qry, bigint = "integer64")$x
  expect_identical(out_int64, bit64::as.integer64(x))

  out_dbl <- bq_table_download(qry, bigint = "numeric")$x
  expect_identical(out_dbl, as.double(x))

  out_chr <- bq_table_download(qry, bigint = "character")$x
  expect_identical(out_chr, x)
})

test_that("can convert geography type", {
  skip_if_not_installed("wk")
  sql <- "SELECT ST_GEOGFROMTEXT('POINT (30 10)') as geography"
  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb)

  expect_identical(df$geography, wk::wkt("POINT(30 10)"))
})

test_that("can convert bytes type", {
  sql <- "SELECT ST_ASBINARY(ST_GEOGFROMTEXT('POINT (30 10)')) as bytes"
  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bq_table_download(tb)

  expect_identical(
    df$bytes,
    blob::blob(
      as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
               0xff, 0xff, 0x3d, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24,
               0x40))
    )
  )
})
