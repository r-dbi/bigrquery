# Individual values -------------------------------------------------------

test_that("can parse atomic vectors", {
  expect_identical(bq_parse_single("x", "string"), "x")
  expect_identical(bq_parse_single("10", "integer"), bit64::as.integer64(10))
  expect_identical(bq_parse_single("10", "float"), 10)
  expect_identical(bq_parse_single("true", "boolean"), TRUE)
  expect_identical(bq_parse_single("false", "boolean"), FALSE)
})

test_that("gracefully handles int64 values", {
  # Largest/smallest 32-bit int
  expect_equal(bq_parse_single("2147483647", "integer"), bit64::as.integer64(2147483647L))
  expect_equal(bq_parse_single("-2147483647", "integer"), bit64::as.integer64(-2147483647L))
  # 2147483647 + 1
  expect_equal(bq_parse_single("2147483648", "integer"), bit64::as.integer64("2147483648"))
  expect_equal(bq_parse_single("-2147483648", "integer"), bit64::as.integer64("-2147483648"))
  # Largest/smallest integer64
  expect_equal(bq_parse_single("9223372036854775807", "integer"), bit64::as.integer64("9223372036854775807"))
  expect_equal(bq_parse_single("-9223372036854775807", "integer"), bit64::as.integer64("-9223372036854775807"))
  # Largest/smallest integer64 + 1
  expect_equal(bq_parse_single("9223372036854775808", "integer"), bit64::as.integer64(NA))
  expect_equal(bq_parse_single("-9223372036854775808", "integer"), bit64::as.integer64(NA))
})

test_that("can parse date/times", {
  d <- as.Date("2018-01-01")
  expect_identical(bq_parse_single(as.character(d), "date"), d)

  dt <- as.POSIXct(d) + 3600 * 3
  attr(dt, "tzone") <- "UTC"

  expect_identical(
    bq_parse_single(as.character(as.integer(dt)), "timestamp"),
    dt
  )
  expect_identical(
    bq_parse_single(as.character(dt, "%Y-%m-%dT%H:%M:%S"), "datetime"),
    dt
  )

  skip_if_not_installed("hms")
  expect_identical(
    bq_parse_single("06:00:00", "time"),
    hms::hms(6 * 3600)
  )
})

test_that("unparseable date-times return NA", {
  skip_on_os("linux")

  expect_equal(
    as.numeric(bq_parse_single("1900-01-01T12:00:00", "datetime")),
    NA_real_
  )
})

test_that("can parse NULLs", {
  expect_identical(bq_parse_single(NULL, "string"), NA_character_)
  expect_identical(bq_parse_single(NULL, "integer"), bit64::as.integer64(NA))
  expect_identical(bq_parse_single(NULL, "float"), NA_real_)
  expect_identical(bq_parse_single(NULL, "boolean"), NA)

  expect_identical(bq_parse_single(NULL, "date"), as.Date(NA))

  na_dtm <- structure(as.POSIXct(NA), tzone = "UTC")
  expect_identical(bq_parse_single(NULL, "datetime"), na_dtm)
  expect_identical(bq_parse_single(NULL, "timestamp"), na_dtm)

  skip_if_not_installed("hms")
  expect_identical(bq_parse_single(NULL, "time"), hms::hms(NA_real_))
})


test_that("can parse arrays of simple values", {
  x <- vs("1", "2", "3")

  out1 <- bq_parse_single(x, "string", mode = "repeated")
  expect_equal(out1, list(c("1", "2", "3")))

  out2 <- bq_parse_single(x, "integer", mode = "repeated")
  expect_equal(out2, list(bit64::as.integer64(1:3)))
})

test_that("can parse structs of simple values", {
  fields <- list(
    bq_field("x", "integer"),
    bq_field("y", "string")
  )

  x <- f(v("1"), v("a"))
  out <- bq_parse_single(x, "record", field = fields)

  expect_equal(out, list(list(x = bit64::as.integer64(1L), y = "a")))
})

test_that("can parse structs of arrays", {
  fields <- list(
    bq_field("x", "integer", "repeated"),
    bq_field("y", "string", "repeated")
  )

  x <- f(v(vs("1", "2", "3")), v(vs("a", "b")))
  out <- bq_parse_single(x, "record", field = fields)

  expect_equal(out, list(list(x = bit64::as.integer64(1:3), y = c("a", "b"))))
})


test_that("can parse arrays of structs", {
  fields <- list(
    bq_field("x", "integer"),
    bq_field("y", "string")
  )

  x <- vs(list(f = vs("1", "a")), list(f = vs("2", "b")))
  out <- bq_parse_single(x, "record", mode = "repeated", field = fields)

  expect_equal(out, list(tibble(x = bit64::as.integer64(1:2), y = c("a", "b"))))
})


# Complete files ----------------------------------------------------------

replay_query <- function(name, sql) {
  schema_path <- test_path(glue("parse-schema-{name}.json"))
  values_path <- test_path(glue("parse-values-{name}.json"))

  if (!file.exists(schema_path)) {
    tbl <- bq_project_query(bq_test_project(), sql)
    bq_table_save_schema(tbl, schema_path)
    bq_table_save_values(tbl, values_path)
  }

  out <- bq_parse_file(schema_path, values_path)
  tibble::as_tibble(out)
}

test_that("can parse nested structures", {
  df <- replay_query("struct", "SELECT STRUCT(1 AS a, 'abc' AS b) as x")
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(df$x[[1]], list(a = bit64::as.integer64(1), b = "abc"))

  df <- replay_query("array", "SELECT [1, 2, 3] as x")
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(df$x[[1]], bit64::as.integer64(1:3))

  df <- replay_query(
    "array-struct",
    "SELECT [STRUCT(1 as a, 'a' as b), STRUCT(2, 'b'), STRUCT(3, 'c')] as x"
  )
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(df$x[[1]], tibble(a = bit64::as.integer64(1:3), b = c("a", "b", "c")))

  df <- replay_query(
    "struct-array",
    "SELECT STRUCT([1, 2, 3] as a, ['a', 'b'] as b) as x"
  )
  expect_named(df, "x")
  expect_type(df$x, "list")
  expect_equal(df$x[[1]], list(a = bit64::as.integer64(1:3), b = c("a", "b")))
})

test_that("can parse empty arrays", {
  tb <- bq_project_query(bq_test_project(), "SELECT ARRAY<INT64>[] as x")
  df <- bq_table_download(tb)
  expect_equal(df$x, list(integer(length = 0)))

  tb <- bq_project_query(bq_test_project(), "SELECT ARRAY<STRUCT<a INT64, b STRING>>[] as x")
  df <- bq_table_download(tb)
  expect_equal(df$x, list(tibble::tibble(a = integer(length = 0), b = character())))
})

test_that("can parse geography", {
  skip_if_not_installed("wk")

  wkt <- wk::wkt("POINT (30 10)")
  expect_identical(bq_parse_single(as.character(wkt), "geography"), wkt)
  expect_identical(bq_parse_single(NA_character_, "geography"), wk::wkt(NA_character_))
})

test_that("can parse bytes", {
  bytes <- blob::blob(
    as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
             0xff, 0xff, 0x3d, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24,
             0x40))
  )

  expect_identical(bq_parse_single(bytes[[1]], "bytes"), bytes)
  expect_identical(
    bq_parse_single(NA_character_, "bytes"),
    blob::blob(NULL)
  )
})

