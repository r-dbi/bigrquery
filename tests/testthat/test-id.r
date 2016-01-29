context("utils")

test_that("basic table name parsing and composition works", {
  project_id <- "myproject"
  table_info <- list(project_id = project_id,
                     dataset_id = "mydataset",
                     table_id = "mytable")

  # Full qualification
  expect_equal(table_info,
               parse_table("myproject:mydataset.mytable"))
  expect_equal(format_table(project = table_info$project_id,
                            dataset = table_info$dataset_id,
                            table = table_info$table_id),
               "myproject:mydataset.mytable")

  # Partial qualification
  expect_equal(table_info,
               parse_table("myproject:mydataset.mytable", project_id = "foo"))
  expect_equal(table_info,
               parse_table("mydataset.mytable", project_id = project_id))
})

test_that("table parsing and composition with domain-scoped projects", {
  project_id <- "mydomain.com:myproject"
  table_info <- list(project_id = project_id,
                     dataset_id = "mydataset",
                     table_id = "mytable")

  # Full qualification
  expect_equal(table_info,
               parse_table("mydomain.com:myproject:mydataset.mytable"))
  expect_equal(format_table(project = table_info$project_id,
                            dataset = table_info$dataset_id,
                            table = table_info$table_id),
               "mydomain.com:myproject:mydataset.mytable")

  # Partial qualification
  expect_equal(table_info,
               parse_table("mydomain.com:myproject:mydataset.mytable",
                           project_id = "someotherproject"))
  expect_equal(table_info,
               parse_table("mydataset.mytable", project_id = project_id))
})

test_that("basic dataset parsing and composition works", {
  project_id <- "myproject"
  dataset_info <- list(project_id = project_id,
                       dataset_id = "mydataset")

  # Full qualification
  expect_equal(dataset_info,
               parse_dataset("myproject:mydataset"))
  expect_equal(format_dataset(project = dataset_info$project_id,
                              dataset = dataset_info$dataset_id),
               "myproject:mydataset")

  # Partial qualification
  expect_equal(dataset_info,
               parse_dataset("myproject:mydataset", project_id = "foo"))
  expect_equal(dataset_info,
               parse_dataset("mydataset", project_id = project_id))
})

test_that("dataset parsing and composition with domain-scoped projects", {
  project_id <- "mydomain.com:myproject"
  dataset_info <- list(project_id = project_id,
                       dataset_id = "mydataset")

  # Full qualification
  expect_equal(dataset_info,
               parse_dataset("mydomain.com:myproject:mydataset"))
  expect_equal(format_dataset(project = dataset_info$project_id,
                              dataset = dataset_info$dataset_id),
               "mydomain.com:myproject:mydataset")

  # Partial qualification
  expect_equal(dataset_info,
               parse_dataset("mydomain.com:myproject:mydataset",
                           project_id = "someotherproject"))
  expect_equal(dataset_info,
               parse_dataset("mydataset", project_id = project_id))
})
