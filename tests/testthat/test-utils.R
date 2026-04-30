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
