test_that("bq_check_namespace() works", {
  expect_no_error(bq_check_namespace("bigrquery", "FIELD_TYPE"))
  expect_snapshot(
    bq_check_namespace("invalid package name", "FIELD_TYPE"),
    error = TRUE
  )
})

test_that("check_labels() accepts valid labels and NULL-like inputs", {
  expect_null(check_labels(NULL))
  expect_null(check_labels(NA))
  expect_null(check_labels(character()))

  expect_equal(check_labels(c(env = "prod")), c(env = "prod"))
  expect_equal(check_labels(c(env = "prod", team = "data")), c(env = "prod", team = "data"))
})

test_that("check_labels() warns and returns NULL for invalid inputs", {
  expect_warning(check_labels(list(env = "prod")), "named character vector")
  expect_warning(check_labels(c("no-name")), "named character vector")
  expect_warning(check_labels(c(ENV = "prod")), "must match")
  expect_warning(check_labels(c(env = "Prod")), "must be empty or match")
})
