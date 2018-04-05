context("table.R")

test_that("can create and delete tables", {
  ds <- bq_test_dataset()

  bq_mtcars <- bq_table(ds, "mtcars")
  expect_false(bq_table_exists(bq_mtcars))

  bq_table_create(bq_mtcars, mtcars)
  expect_true(bq_table_exists(bq_mtcars))

  bq_table_delete(bq_mtcars)
  expect_false(bq_table_exists(bq_mtcars))
})

test_that("can copy table from public dataset", {
  ds <- bq_test_dataset()
  my_natality <- bq_table(ds, "mynatality")

  out <- bq_table_copy("publicdata.samples.natality", my_natality)
  expect_equal(out, my_natality)
  expect_true(bq_table_exists(my_natality))
})

# Old API -----------------------------------------------------------------
