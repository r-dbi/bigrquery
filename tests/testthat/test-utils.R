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
  expect_null(check_labels(list()))

  expect_equal(check_labels(list(env = "prod")), list(env = "prod"))
  expect_equal(check_labels(list(env = "prod", team = "data")), list(env = "prod", team = "data"))
})

test_that("check_labels() warns and returns NULL for invalid inputs", {
  expect_warning(check_labels("not-a-list"), "dictionary list")
  expect_warning(check_labels(list("no-name")), "non-empty strings")
  expect_warning(check_labels(list(ENV = "prod")), "must match")
  expect_warning(check_labels(list(env = "Prod")), "must be empty or match")
})
