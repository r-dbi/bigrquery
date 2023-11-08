test_that("job coercion equivalent to construction", {
  ref <- bq_job("a", "b", "c")

  expect_equal(bq_job("a", "b", "c"), ref)
  expect_equal(as_bq_job("a.b.c"), ref)
  x <- list(projectId = "a", jobId = "b", location = "c")
  expect_equal(as_bq_job(x), ref)
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
  expect_snapshot({
    as_bq_job("x.y.US")
    as_bq_dataset("x.y")
    as_bq_table("x.y.z")
  })
})

test_that("string coercion error on invalid number of components", {
  expect_snapshot(error = TRUE, {
    as_bq_table("x")
    as_bq_table("a.b.c.d")
    as_bq_job("x")
    as_bq_dataset("x")
  })
})

test_that("list coercion errors with bad names", {
  expect_snapshot(error = TRUE, {
    as_bq_table(list())
    as_bq_dataset(list())
    as_bq_job(list())
  })
})
