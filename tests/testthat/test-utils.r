context("utils")

test_that("basic table name parsing works", {
  project_id <- "myproject"
  table_info <- list(project_id = project_id,
                     dataset_id = "mydataset",
                     table_id = "mytable")
  expect_equal(table_info,
               parse_table("myproject:mydataset.mytable"))
  expect_equal(table_info,
               parse_table("myproject:mydataset.mytable", project_id = "foo"))
  expect_equal(table_info,
               parse_table("mydataset.mytable", project_id = project_id))
})

test_that("table parsing with domain-scoped projects", {
  project_id <- "mydomain.com:myproject"
  table_info <- list(project_id = project_id,
                     dataset_id = "mydataset",
                     table_id = "mytable")
  expect_equal(table_info,
               parse_table("mydomain.com:myproject:mydataset.mytable"))
  expect_equal(table_info,
               parse_table("mydomain.com:myproject:mydataset.mytable",
                           project_id = "someotherproject"))
  expect_equal(table_info,
               parse_table("mydataset.mytable", project_id = project_id))
})

test_that("basic dataset parsing works", {
  project_id <- "myproject"
  dataset_info <- list(project_id = project_id,
                     dataset_id = "mydataset")
  expect_equal(dataset_info,
               parse_dataset("myproject:mydataset"))
  expect_equal(dataset_info,
               parse_dataset("myproject:mydataset", project_id = "foo"))
  expect_equal(dataset_info,
               parse_dataset("mydataset", project_id = project_id))
})

test_that("dataset parsing with domain-scoped projects", {
  project_id <- "mydomain.com:myproject"
  dataset_info <- list(project_id = project_id,
                     dataset_id = "mydataset")
  expect_equal(dataset_info,
               parse_dataset("mydomain.com:myproject:mydataset"))
  expect_equal(dataset_info,
               parse_dataset("mydomain.com:myproject:mydataset",
                           project_id = "someotherproject"))
  expect_equal(dataset_info,
               parse_dataset("mydataset", project_id = project_id))
})
