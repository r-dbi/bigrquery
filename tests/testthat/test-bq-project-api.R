test_that("public datasets includes baseball", {
  skip_if_not(bq_authable())

  public <- suppressWarnings(bq_project_datasets("bigquery-public-data"))
  names <- map_chr(public, function(x) x$dataset)

  expect_true("baseball" %in% names)
})

test_that("test project has at least one job", {
  jobs <- bq_project_jobs(bq_test_project(), warn = FALSE)
  expect_gte(length(jobs), 1)
  expect_s3_class(jobs[[1]], "bq_job")
})
