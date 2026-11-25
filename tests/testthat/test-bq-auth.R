test_that("bq_user() works", {
  skip_if_no_auth()
  expect_match(bq_user(), "@.*[.]iam[.]gserviceaccount[.]com")
})

test_that("useful error if can't auth", {
  local_mocked_bindings(token_fetch = function(...) NULL)

  expect_snapshot(bq_auth(), error = TRUE)
})

test_that("bq_auth_configure checks its inputs", {
  expect_snapshot(error = TRUE, {
    bq_auth_configure(1, 1)
    bq_auth_configure(client = 1)
    bq_auth_configure(path = 1)
  })
})
