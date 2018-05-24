context("test-bq-job.R")

test_that("can control chattiness of bq_job_wait", {
  job <- bq_project_jobs(bq_test_project(), max_pages = 1, warn = FALSE)[[1]]

  expect_message(bq_job_wait(job, quiet = TRUE), NA)
  expect_message(bq_job_wait(job, quiet = FALSE), "Complete")
  expect_message(bq_job_wait(job, quiet = NA), if (interactive()) "Complete" else NA)
})
