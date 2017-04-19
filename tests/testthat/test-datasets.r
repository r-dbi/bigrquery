context("datasets")

test_that("extra arguments passed onto request", {
  skip_if_no_auth()

  insert_dataset("bigrquery-examples", "test_temp",
    friendlyName = "xxx",
    description = "yyy"
  )
  on.exit(delete_dataset("bigrquery-examples", "test_temp"))

  ds <- get_dataset("bigrquery-examples", "test_temp")
  expect_equal(ds$friendlyName, "xxx")
  expect_equal(ds$description, "yyy")
})

test_that("list_datasets validates correctly", {
  expect_error(list_datasets(1),
               ".*project is not a string.*")
})

test_that("get_dataset validates correctly", {
  expect_error(get_dataset(1, 2),
               ".*project is not a string.*")
  expect_error(get_dataset(1, "string"),
               ".*project is not a string.*")
  expect_error(get_dataset("string", 1),
               ".*dataset is not a string.*")
})

test_that("delete_dataset validates correctly", {
  expect_error(delete_dataset(1, 2),
               ".*project is not a string.*")
  expect_error(delete_dataset(1, "string"),
               ".*project is not a string.*")
  expect_error(delete_dataset("string", 1),
               ".*dataset is not a string.*")
})

test_that("insert_dataset validates correctly", {
  expect_error(insert_dataset(1, 2),
               ".*project is not a string.*")
  expect_error(insert_dataset(1, "string"),
               ".*project is not a string.*")
  expect_error(insert_dataset("string", 1),
               ".*dataset is not a string.*")
})


test_that("update_dataset validates correctly", {
  expect_error(update_dataset(1, 2),
               ".*project is not a string.*")
  expect_error(update_dataset(1, "string"),
               ".*project is not a string.*")
  expect_error(update_dataset("string", 1),
               ".*dataset is not a string.*")
})
