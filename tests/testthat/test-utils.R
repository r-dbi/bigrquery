test_that("bq_check_namespace() works", {
  expect_no_error(bq_check_namespace("bigrquery", "FIELD_TYPE"))
  expect_snapshot(
    bq_check_namespace("invalid package name", "FIELD_TYPE"),
    error = TRUE
  )
})

test_that("cli_escape() doubles cli braces", {
  expect_equal(cli_escape("no braces"), "no braces")
  expect_equal(cli_escape("{x}"), "{{x}}")
})

test_that("check_labels() accepts valid labels and NULL-like inputs", {
  expect_no_error(check_labels(NULL))
  expect_no_error(check_labels(list()))
  expect_no_error(check_labels(list(env = "prod")))
  expect_no_error(check_labels(list(env = "prod", team = "data")))
  expect_no_error(check_labels(list(env = "")))
})

test_that("check_labels() errors on invalid inputs", {
  expect_snapshot(error = TRUE, {
    check_labels(c(env = "prod"))
    check_labels(list("no-name"))
    check_labels(list(env = 1))
  })
})
