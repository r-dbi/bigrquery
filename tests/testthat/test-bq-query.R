test_that("bq_project_query inputs are checked", {
  expect_snapshot(error = TRUE, {
    bq_project_query(1)
    bq_project_query("abc", 1)
    bq_project_query("abc", "SELECT *", destination_table = 1)
    bq_project_query("abc", "SELECT *", destination_table = "a")
    bq_project_query("abc", "SELECT *", destination_table = list())
    bq_project_query("abc", "SELECT *", quiet = 1)
  })
})

test_that("bq_dataset_query inputs are checked", {
  expect_snapshot(error = TRUE, {
    bq_dataset_query(1)
    bq_dataset_query("abc")
    bq_dataset_query("abc.def", 1)
    bq_dataset_query("abc.def", "SELECT *", destination_table = 1)
    bq_dataset_query("abc.def", "SELECT *", destination_table = "a")
    bq_dataset_query("abc.def", "SELECT *", destination_table = list())
    bq_dataset_query("abc.def", "SELECT *", billing = 1)
    bq_dataset_query("abc.def", "SELECT *", quiet = 1)
  })
})
