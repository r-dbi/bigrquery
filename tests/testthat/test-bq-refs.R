test_that("job coercion equivalent to construction", {
  ref <- bq_job("a", "b", "c")
  l <- list(projectId = "a", jobId = "b", location = "c")

  expect_equal(as_bq_job("a.b.c"), ref)
  expect_equal(as_bq_job(l), ref)
  expect_equal(as_bq_job(ref), ref)
})

test_that("dataset coercion equivalent to construction", {
  ref <- bq_dataset("a", "b")
  l <- list(projectId = "a", datasetId = "b")

  expect_equal(as_bq_dataset("a.b"), ref)
  expect_equal(as_bq_dataset(l), ref)
  expect_equal(as_bq_dataset(ref), ref)
})

test_that("table equivalent to construction", {
  ref <- bq_table("a", "b", "c")
  l <- list(projectId = "a", datasetId = "b", tableId = "c")

  expect_equal(as_bq_table("a.b.c"), ref)
  expect_equal(as_bq_table(l), ref)
  expect_equal(as_bq_table(ref), ref)
})

test_that("can make table with dataset", {
  ds <- bq_dataset("a", "b")
  expect_equal(bq_table(ds, "c"), bq_table("a", "b", "c"))

  expect_snapshot(bq_table(ds, 1), error = TRUE)
})

test_that("objects have helpful print methods", {
  expect_snapshot({
    as_bq_job("x.y.US")
    as_bq_dataset("x.y")
    as_bq_table("x.y.z")
  })
})

test_that("useful error for non-strings", {
  expect_snapshot(error = TRUE, {
    as_bq_job(1)
    as_bq_dataset(1)
    as_bq_table(1)
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
