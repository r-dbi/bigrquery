context("test-bq-param.R")

test_that("can create parameters from list", {
  x <- list(a = bq_param(1, "integer"), b = "x")
  p <- as_bq_params(x)

  expect_length(p, 2)
  expect_equal(p[[1]], x[[1]])
  expect_equal(p[[2]], bq_param("x"))
})
