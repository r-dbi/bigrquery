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

test_that("parsing with domain-scoped projects", {
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
