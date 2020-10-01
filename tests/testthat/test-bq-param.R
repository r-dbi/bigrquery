test_that("can create parameters from list", {
  x <- list(a = bq_param(1, "integer"), b = "x", c = 1:3)
  p <- as_bq_params(x)

  expect_length(p, 3)
  expect_equal(p[[1]], bq_param_scalar(1, "integer", name = "a"))
  expect_equal(p[[2]], bq_param_scalar("x", name = "b"))
  expect_equal(p[[3]], bq_param_array(1:3, name = "c"))
})

test_that("parameter json doesn't change without notice", {
  verify_output(test_path("test-bq-param-json.txt"), {
    as_bq_params(list(
      scalar = "a",
      vector = c("a", "b", "c")
    ))
  })
})
