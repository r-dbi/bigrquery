context("test-bq-parse.R")

# Individual values -------------------------------------------------------

test_that("can parse atomic vectors", {
  expect_identical(bq_parse_single("x", "string"), "x")
  expect_identical(bq_parse_single("10", "integer"), 10L)
  expect_identical(bq_parse_single("10", "float"), 10)
  expect_identical(bq_parse_single("TRUE", "boolean"), TRUE)
  expect_identical(bq_parse_single("FALSE", "boolean"), FALSE)
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
    bq_parse_single(as.character(dt), "datetime"),
    dt
  )

  expect_identical(
    bq_parse_single("06:00:00", "time"),
    as.difftime(6 * 3600, units = "secs")
  )
})

test_that("can parse NULLs", {
  expect_identical(bq_parse_single(NULL, "string"), NA_character_)
  expect_identical(bq_parse_single(NULL, "integer"), NA_integer_)
  expect_identical(bq_parse_single(NULL, "float"), NA_real_)
  expect_identical(bq_parse_single(NULL, "boolean"), NA)

  expect_identical(bq_parse_single(NULL, "date"), as.Date(NA))

  na_dtm <- structure(as.POSIXct(NA), tzone = "UTC")
  expect_identical(bq_parse_single(NULL, "datetime"), na_dtm)
  expect_identical(bq_parse_single(NULL, "timestamp"), na_dtm)
  expect_identical(bq_parse_single(NULL, "time"), as.difftime(NA_real_, units = "secs"))
})


test_that("can parse arrays of simple values", {
  x <- vs("1", "2", "3")

  out1 <- bq_parse_single(x, "string", mode = "repeated")
  expect_equal(out1, list(c("1", "2", "3")))

  out2 <- bq_parse_single(x, "integer", mode = "repeated")
  expect_equal(out2, list(1:3))
})

test_that("can parse structs of simple values", {
  fields <- list(
    bq_field("x", "integer"),
    bq_field("y", "string")
  )

  x <- f(v("1"), v("a"))
  out <- bq_parse_single(x, "record", field = fields)

  expect_equal(out, list(list(x = 1L, y = "a")))
})

test_that("can parse structs of arrays", {
  fields <- list(
    bq_field("x", "integer", "repeated"),
    bq_field("y", "string", "repeated")
  )

  x <- f(v(vs("1", "2", "3")), v(vs("a", "b")))
  out <- bq_parse_single(x, "record", field = fields)

  expect_equal(out, list(list(x = 1:3, y = c("a", "b"))))
})


test_that("can parse arrays of structs", {
  fields <- list(
    bq_field("x", "integer"),
    bq_field("y", "string")
  )

  x <- vs(list(f = vs("1", "a")), list(f = vs("2", "b")))
  out <- bq_parse_single(x, "record", mode = "repeated", field = fields)

  expect_equal(out, list(data_frame(x = 1:2, y = c("a", "b"))))
})


# Complete files ----------------------------------------------------------

test_that("can parse table of simple values", {
  df <- data_frame(
    x = 1:5,
    y = letters[5:1],
    z = as.Date("2018-01-01") + 0:4
  )
  # ds <- bq_test_dataset()
  # tb <- bq_table(ds, "simple")
  # bq_table_upload(tb, df)
  # bq_table_save_fields(tb, "tests/testthat/field-simple.json")
  # bq_table_save_data(tb, "tests/testthat/data-simple.json")

  out <- bq_parse_file(test_path("field-simple.json"), test_path("data-simple.json"))
  out <- out[order(out$x), ]
  rownames(out) <- NULL

  expect_equal(out, df)
})
