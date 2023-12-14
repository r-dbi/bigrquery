test_that("same results regardless of page size", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")

  df3 <- bq_table_download(tb, n_max = 30, page_size = 10)
  df1 <- bq_table_download(tb, n_max = 30, page_size = 30)
  expect_equal(nrow(df1), 30)
  expect_equal(df1, df3)
})

test_that("can retrieve fraction of page size", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, n_max = 15, page_size = 10)
  expect_equal(nrow(df), 15)
})

test_that("can retrieve zero rows", {
  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, n_max = 0)
  expect_equal(nrow(df), 0)
  expect_named(df, c("phase", "phase_emoji", "peak_datetime"))
})

test_that("can specify large integers in page params", {
  skip_if_no_auth()

  # Use scipen to nudge R to prefer scientific formatting for very small numbers
  # to allow this test to exercise issue #395 with small datasets.
  withr::local_options(list(scipen = -4))

  tb <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  df <- bq_table_download(tb, n_max = 100, page_size = 20)
  expect_equal(nrow(df), 100)
})

test_that("errors when table is known to be incomplete", {
  # If this snapshot changes, it's likely because the table has been updated

  skip_if_no_auth()

  tb <- as_bq_table("bigquery-public-data.chicago_taxi_trips.taxi_trips")
  expect_snapshot(
    bq_table_download(
      tb,
      n_max = 35000,
      page_size = 35000,
      bigint = "integer64"
    ),
    transform = function(x) {
      gsub("[0-9,]+ rows were received", "{n} rows were received", x, perl = TRUE)
    },
    error = TRUE
  )
})

# helpers around row and chunk params ------------------------------------------

test_that("set_row_params() works ", {
  # n_max > nrow
  expect_equal(
    set_row_params(10, n_max = 15),
    list(n_max = 10, start_index = 0)
  )
  # start_index > nrow
  expect_equal(
    set_row_params(10, start_index = 12),
    list(n_max = 0, start_index = 12)
  )
  expect_equal(
    set_row_params(10, n_max = 5, start_index = 12),
    list(n_max = 0, start_index = 12)
  )
  # n_max > nrow - start_index
  expect_equal(
    set_row_params(10, n_max = 5, start_index = 7),
    list(n_max = 3, start_index = 7)
  )
})

test_that("set_chunk_params() works", {
  # no chunk_size, no n_chunks
  expect_equal(set_chunk_params(5), list(chunk_size = 5, n_chunks = 1))

  # yes chunk_size, no n_chunks
  expect_equal(
    set_chunk_params(10, chunk_size = 3),
    list(chunk_size = 3, n_chunks = 4)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 10),
    list(chunk_size = 10, n_chunks = 1)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 9),
    list(chunk_size = 9, n_chunks = 2)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 15),
    list(chunk_size = 10, n_chunks = 1)
  )

  # no chunk_size, yes n_chunks
  expect_equal(
    set_chunk_params(10, n_chunks = 1),
    list(chunk_size = 10, n_chunks = 1)
  )
  expect_equal(
    set_chunk_params(10, n_chunks = 3),
    list(chunk_size = 4, n_chunks = 3)
  )
  expect_equal(
    set_chunk_params(10, n_chunks = 10),
    list(chunk_size = 1, n_chunks = 10)
  )
  expect_equal(
    set_chunk_params(10, n_chunks = 11),
    list(chunk_size = 1, n_chunks = 10)
  )

  # yes chunk_size, yes n_chunks
  expect_equal(
    set_chunk_params(10, chunk_size = 3, n_chunks = 1),
    list(chunk_size = 3, n_chunks = 1)
  )
  expect_equal(
    set_chunk_params(10, chunk_size = 7, n_chunks = 5),
    list(chunk_size = 7, n_chunks = 2)
  )
})

test_that("set_chunk_plan() works", {
  dat <- set_chunk_plan(5, chunk_size = 5, n_chunks = 1)
  expect_equal(dat$chunk_begin, 0)
  expect_equal(dat$chunk_rows, 5)

  dat <- set_chunk_plan(5, chunk_size = 5, n_chunks = 1, start_index = 3)
  expect_equal(dat$chunk_begin, 3)
  expect_equal(dat$chunk_rows, 5)

  dat <- set_chunk_plan(10, chunk_size = 3, n_chunks = 4)
  expect_equal(dat$chunk_begin, c(0, 3, 6, 9))
  expect_equal(dat$chunk_rows, c(3, 3, 3, 1))

  dat <- set_chunk_plan(10, chunk_size = 3, n_chunks = 4, start_index = 8)
  expect_equal(dat$chunk_begin, 8 + c(0, 3, 6, 9))
  expect_equal(dat$chunk_rows, c(3, 3, 3, 1))
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


test_that("can parse fractional seconds", {
  x <- c(
    "2000-01-02T03:04:05.67",
    "2000-01-02T03:04:05",
    "2000-01-02T03:04:05.123456"
  )
  parsed <- as.numeric(bq_datetime_parse(x))
  expect_equal(parsed - trunc(parsed), c(0.67, 0, 0.123456), tolerance = 1e-6)
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
