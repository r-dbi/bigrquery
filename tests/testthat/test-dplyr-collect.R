test_that("collect can identify directly download tables", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  bq1 <- dplyr::tbl(con, "mtcars")
  expect_true(op_can_download(bq1))
  expect_equal(op_rows(bq1), Inf)
  expect_equal(format(op_table(bq1)), "`mtcars`")

  bq2 <- head(bq1, 4)
  expect_true(op_can_download(bq2))
  expect_equal(op_rows(bq2), 4)
  expect_equal(format(op_table(bq1)), "`mtcars`")

  bq3 <- head(bq2, 2)
  expect_true(op_can_download(bq3))
  expect_equal(op_rows(bq3), 2)

  bq3 <- dplyr::filter(bq1, cyl == 1)
  expect_false(op_can_download(bq3))

  x <- dplyr::collect(bq1, api = "arrow")
  expect_s3_class(x, "tbl")

  bq5 <- dplyr::tbl(con, dplyr::sql("SELECT * FROM mtcars"))
  expect_false(op_can_download(bq5))
  expect_false(op_can_download(head(bq5)))

  view_name <- random_name()
  DBI::dbExecute(
    con,
    paste0("CREATE VIEW ", view_name, " AS SELECT * FROM basedata.mtcars")
  )
  defer(DBI::dbExecute(con, paste0("DROP VIEW ", view_name)))
  bq6 <- dplyr::tbl(con, view_name)
  expect_false(op_can_download(bq6))

  # INFORMATION_SCHEMA.TABLES errors when we attempt to get metadata
  bq7 <- dplyr::tbl(con, "INFORMATION_SCHEMA.PARTITIONS")
  expect_false(op_can_download(bq7))
  expect_false(op_can_download(head(bq7)))
})
