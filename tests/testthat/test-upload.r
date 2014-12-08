context("upload")

test_that("ints and strings are correctly encoded as CSV", {
  df <- data.frame(ints = c(1, NA, 3, 11), strs = c('a"b', '', NA, 'abc'))
  df_csv <- standard_csv(df)
  expected_csv <- '1,"a""b"\n,""\n3,\n11,"abc"'

  expect_equal(df_csv, expected_csv)
})
