test_that("bq_user() works", {
  skip_if_no_auth()
  expect_match(bq_user(), "@.*[.]iam[.]gserviceaccount[.]com")
})
