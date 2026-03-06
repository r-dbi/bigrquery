test_that("bq_check_namespace() works", {
  expect_no_error(bq_check_namespace("bigrquery", "FIELD_TYPE"))
  expect_snapshot(
    bq_check_namespace("invalid package name", "FIELD_TYPE"),
    error = TRUE
  )
})

test_that("check_labels() accepts valid labels and NULL-like inputs", {
  expect_null(check_labels(NULL))
  expect_null(check_labels(list()))

  expect_equal(check_labels(list(env = "prod")), list(env = "prod"))
  expect_equal(check_labels(list(env = "prod", team = "data")), list(env = "prod", team = "data"))
  expect_equal(check_labels(list(env = "")), list(env = ""))
})

test_that("check_labels() errors on invalid inputs", {
  expect_error(check_labels(c(env = "prod")), "named list")
  expect_error(check_labels(list("no-name")), "named list")
})
