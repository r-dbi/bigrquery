context("test-bq-job.R")

test_that("job coercion equivalent to construction", {
  expect_equal(bq_job("a", "b"), as_bq_job("a.b"))

  x <- list(projectId = "a", jobId = "b")
  expect_equal(bq_job("a", "b"), as_bq_job(x))
})

test_that("dataset coercion equivalent to construction", {
  expect_equal(bq_dataset("a", "b"), as_bq_dataset("a.b"))

  x <- list(projectId = "a", datasetId = "b")
  expect_equal(bq_dataset("a", "b"), as_bq_dataset(x))
})

test_that("table equivalent to construction", {
  expect_equal(bq_table("a", "b", "c"), as_bq_table("a.b.c"))

  x <- list(projectId = "a", datasetId = "b", tableId = "c")
  expect_equal(bq_table("a", "b", "c"), as_bq_table(x))
})
