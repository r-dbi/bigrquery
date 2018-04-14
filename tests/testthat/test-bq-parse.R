context("test-bq-parse.R")

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

test_that("can parse arrays of simple values", {
  x <- vs("1", "2", "3")

  out1 <- bq_parse_single(x, "string", mode = "repeated")
  expect_equal(out1, list(c("1", "2", "3")))

  out2 <- bq_parse_single(x, "integer", mode = "repeated")
  expect_equal(out2, list(1:3))
})
