context("test-bq-job.R")

test_that("coercion equivalent to construction", {
  expect_equal(bq_job("a", "b"), as_bq_job("a.b"))
  expect_equal(bq_job("a", "b"), as_bq_job(list(projectId = "a", jobId = "b")))
})
