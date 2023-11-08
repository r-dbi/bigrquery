test_that("available projects should include test project", {
  proj <- bq_test_project()
  expect_true(proj %in% bq_projects())
})
