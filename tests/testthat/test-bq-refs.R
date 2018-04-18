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

test_that("objects have helpful print methods", {
  expect_known_output({
    print(as_bq_job("x.y"))
    print(as_bq_dataset("x.y"))
    print(as_bq_table("x.y.z"))
  }, file = test_path("bg-refs-print.txt"))
})

test_that("string coercion error on invalid number of components", {
  expect_error(as_bq_table("x"), "must contain 3")
  expect_error(as_bq_table("a.b.c.d"), "must contain 3")
  expect_error(as_bq_job("x"), "must contain 2")
  expect_error(as_bq_dataset("x"), "must contain 2")
})

test_that("list coercion errors with bad names", {
  expect_error(as_bq_table(list()), "must have components")
  expect_error(as_bq_dataset(list()), "must have components")
  expect_error(as_bq_job(list()), "must have components")
})
