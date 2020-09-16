
test_that("bq_check_namespace() works", {
  expect_identical(bq_check_namespace("bigrquery", "FIELD_TYPE"), "bigrquery")
  expect_error(
    bq_check_namespace("invalid package name", "FIELD_TYPE"),
    "must be installed"
  )
})
