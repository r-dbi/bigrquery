context("test-bq-param.R")

test_that("can create parameters from list", {
  x <- list(a = bq_param(1, "integer"), b = "x")
  p <- as_bq_params(x)

  expect_length(p, 2)
  expect_equal(p[[1]], bq_param_scalar(1, "integer", name = "a"))
  expect_equal(p[[2]], bq_param_scalar("x", name = "b"))
})


test_that("json structure is correct for query parameters", {
  scalar.param.value <- "a"
  scalar.param <- bq_param(c(scalar.param.value))
  vector.param.values <- c("a", "b", "c")
  vector.param <- bq_param(vector.param.values)

  x <- as_bq_params(
    list(
      param_scalar = scalar.param,
      param_vector = vector.param
    )
  )

  res <- as_json(x)

  expectation <- list(
    list(
      name = "param_scalar",
      parameterType = list(
        type = unbox("STRING")
      ),
      parameterValue = list(
        value = unbox(scalar.param.value)
      )
    ),
    list(
      name = "param_vector",
      parameterType = list(
        type = "ARRAY",
        arrayType = list(
          type = unbox("STRING")
        )
      ),
      parameterValue = list(
        arrayValues = list(
          list(value = unbox("a")),
          list(value = unbox("b")),
          list(value = unbox("c"))
        )
      )
    )
  )
  expect_equal(res, expectation)
})


test_that("json structure is correct for query parameters", {
  scalar.param.value <- "a"
  scalar.param <- bq_param(c(scalar.param.value))
  vector.param.values <- c("a", "b", "c")
  vector.param <- bq_param(vector.param.values)

  x <- as_bq_params(
    list(
      param_scalar = scalar.param,
      param_vector = vector.param
    )
  )

  res <- as_json(x)

  expectation <- list(
    list(
      name = "param_scalar",
      parameterType = list(
        type = unbox("STRING")
      ),
      parameterValue = list(
        value = unbox(scalar.param.value)
      )
    ),
    list(
      name = "param_vector",
      parameterType = list(
        type = "ARRAY",
        arrayType = list(
          type = unbox("STRING")
        )
      ),
      parameterValue = list(
        arrayValues = list(
          list(value = unbox("a")),
          list(value = unbox("b")),
          list(value = unbox("c"))
        )
      )
    )
  )
  expect_equal(res, expectation)
})
