context("test-bq-field.R")

test_that("can create and coerce a single field", {
  x1 <- bq_field("x", "string")
  x2 <- as_bq_field(list(name = "x", type = "string"))

  expect_s3_class(x1, "bq_field")
  expect_equal(x1$name, "x")
  expect_equal(x1, x2)
})

test_that("can compute fields from data frame", {
  df <- data.frame(x = 1, y = "a")
  fs <- as_bq_fields(df)

  expect_length(fs, 2)
  expect_equal(fs[[1]], bq_field("x", "float"))
  expect_equal(fs[[2]], bq_field("y", "string"))
})
