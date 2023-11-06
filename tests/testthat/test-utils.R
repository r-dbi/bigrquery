
test_that("bq_check_namespace() works", {
  expect_no_error(bq_check_namespace("bigrquery", "FIELD_TYPE"))
  expect_snapshot(
    bq_check_namespace("invalid package name", "FIELD_TYPE"),
    error = TRUE
  )
})
