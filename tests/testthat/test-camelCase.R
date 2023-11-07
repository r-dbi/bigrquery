test_that("toCamel recursively renamed", {
  x1 <- list(a_b = 1, c_d = list(e_f = 2))
  x2 <- list(aB = 1, cD = list(eF = 2))

  expect_equal(toCamel(x1), x2)
})

test_that("unnamed objects left as is", {
  x <- 1:5
  expect_null(names(toCamel(x)))
})
