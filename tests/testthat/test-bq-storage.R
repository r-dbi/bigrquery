# BigQueryStorage -------------------------------------------------------

test_that("BigQuery json and BigQuery return the same results", {

	# Compare with bigrquery method
	dt <- bqs_table_download("bigquery-public-data.usa_names.usa_1910_current", bq_test_project(), max_results = 50000, as_tibble = TRUE)
	dt2 <- bigrquery::bq_table_download("bigquery-public-data.usa_names.usa_1910_current", max_results = 50000)
	expect_equal(dt, dt2)

})

test_that("Method dispatch to BigQuery Storage", {

  con <- bigrquery::dbConnect(
    bigrquery::bigquery(),
    project = "bigquery-public-data",
    dataset = "usa_names",
    billing = bq_test_project())

  # Basic reading table as data.frame
  dt <- DBI::dbGetQuery(con, "SELECT * FROM `bigquery-public-data.usa_names.usa_1910_current` LIMIT 50000", bqs = TRUE)
  expect_true(inherits(dt, "data.frame"))
  expect_true(nrow(dt) == 50000)

  # Full table fetch
  dt <- DBI::dbReadTable(con, "bigquery-public-data.usa_names.usa_1910_current", bqs = TRUE)
  expect_true(inherits(dt, "data.frame"))

})

test_that("0 rows table can be returned", {

  con <- bigrquery::dbConnect(
    bigrquery::bigquery(),
    project = "bigquery-public-data",
    dataset = "usa_names",
    billing = bq_test_project())

  # Check if a 0 rows table can be returned
  dt <- DBI::dbGetQuery(con, "SELECT * FROM `bigquery-public-data.usa_names.usa_1910_current` LIMIT 0", bqs = TRUE)
  expect_true(inherits(dt, "data.frame"))
  expect_true(nrow(dt) == 0)

})

test_that("Optional BigQuery Storage API parameters work", {
  # Check other features
  dt <- bqs_table_download("bigquery-public-data:usa_names.usa_1910_current",
                           bq_test_project(),
                           selected_fields = c("name", "number", "state"),
                           row_restriction = 'state = "WA"')
  expect_length(names(dt), 3)
  expect_identical(as.character(unique(dt$state)), "WA")
})

# types -------------------------------------------------------------------

test_that("can read utf-8 strings", {
  sql <- "SELECT '\U0001f603' as x"
  tb <- bq_project_query(bq_test_project(), sql)
  df <- bqs_table_download(tb, bq_test_project(), as_tibble = TRUE)
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
  df <- bqs_table_download(tb, bq_test_project(), as_tibble = TRUE)

  base <- ISOdatetime(2000, 1, 2, 3, 4, 5.67, tz = "UTC")

  expect_equal(df$datetime, base)
  expect_equal(df$timestamp, base)
  expect_equal(df$date, as.Date(base))
  expect_equal(df$time, hms::hms(hours = 3, minutes = 4, seconds = 5.67))
})

test_that("correctly parse logical values" ,{
  query <- "SELECT TRUE as x"
  tb <- bq_project_query(bq_test_project(), query)
  df <- bqs_table_download(tb, bq_test_project(), as_tibble = TRUE)

  expect_true(df$x)
})

test_that("the return type of integer columns is set by the bigint argument", {
  x <- c("-2147483648", "-2147483647", "-1", "0", "1", "2147483647", "2147483648")
  sql <- paste0("SELECT * FROM UNNEST ([", paste0(x, collapse = ","), "]) AS x");
  qry <- bq_project_query(bq_test_project(), sql)

  expect_warning(
    out_int <- bqs_table_download(qry, bq_test_project(), as_tibble = TRUE, bigint = "integer")$x,
    "integer overflow"
  )
  expect_identical(out_int, suppressWarnings(as.integer(x)))

  out_int64 <- bqs_table_download(qry, bq_test_project(), as_tibble = TRUE, bigint = "integer64")$x
  expect_identical(out_int64, bit64::as.integer64(x))

  out_dbl <- bqs_table_download(qry, bq_test_project(), as_tibble = TRUE, bigint = "numeric")$x
  expect_identical(out_dbl, as.double(x))

  out_chr <- bqs_table_download(qry, bq_test_project(), as_tibble = TRUE, bigint = "character")$x
  expect_identical(out_chr, x)
})

test_that("can convert geography type", {
  skip_if_not_installed("wk")
  sql <- "SELECT ST_GEOGFROMTEXT('POINT (30 10)') as geography"
  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bqs_table_download(tb, bq_test_project(), as_tibble = TRUE)

  expect_identical(df$geography, wk::wkt("POINT(30 10)"))
})

test_that("can convert bytes type", {
  sql <- "SELECT ST_ASBINARY(ST_GEOGFROMTEXT('POINT (30 10)')) as bytes"
  tb <- bq_project_query(bq_test_project(), sql, quiet = TRUE)
  df <- bqs_table_download(tb, bq_test_project(), as_tibble = TRUE)

  expect_identical(
    df$bytes,
    blob::blob(
      as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
               0xff, 0xff, 0x3d, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24,
               0x40))
    )
  )
})
