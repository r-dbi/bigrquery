test_that("historical API continues to work", {
  src <- src_bigquery(bq_test_project(), "basedata")

  x <- dplyr::tbl(src, "mtcars")

  expect_s3_class(x, "tbl")
  expect_true("cyl" %in% dbplyr::op_vars(x))
})

test_that("can work with literal SQL", {
  con_us <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "utility_us",
    billing = bq_test_project()
  )
  x <- dplyr::tbl(con_us, dplyr::sql("SELECT * FROM country_code_iso"))

  expect_s3_class(x, "tbl")
  expect_true("fips_code" %in% dbplyr::op_vars(x))
})

test_that("can copy_to", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(ds)

  expect_snapshot(dplyr::copy_to(con, mtcars), error = TRUE)
  bq_mtcars <- dplyr::copy_to(con, mtcars, temporary = FALSE)

  expect_s3_class(bq_mtcars, "tbl_BigQueryConnection")
})

test_that("can collect and compute (no dataset)", {
  con <- DBI::dbConnect(bigquery(), project = bq_test_project())
  bq_mtcars <- dplyr::tbl(con, "basedata.mtcars") %>% dplyr::filter(cyl == 4)

  # collect
  x <- dplyr::collect(bq_mtcars)
  expect_equal(dim(x), c(11, 11))

  # compute: temporary
  temp <- dplyr::compute(bq_mtcars)
  expect_equal(dim(dplyr::collect(temp)), c(11, 11))

  # compute: persistent
  name <- paste0("basedata.", random_name())
  if (packageVersion("dbplyr") >= "2.4.0.9000") name <- I(name)
  perm <- dplyr::compute(bq_mtcars, name = name, temporary = FALSE)
  defer(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
  expect_equal(dim(dplyr::collect(perm)), c(11, 11))
})

test_that("can collect and compute (with dataset)", {
  con <- DBI::dbConnect(bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )
  bq_mtcars <- dplyr::tbl(con, "mtcars") %>% dplyr::filter(cyl == 4)

  # collect
  x <- dplyr::collect(bq_mtcars)
  expect_equal(dim(x), c(11, 11))

  # compute: temporary
  temp <- dplyr::compute(bq_mtcars)
  expect_equal(dim(dplyr::collect(temp)), c(11, 11))

  # compute: persistent
  name <- random_name()
  perm <- dplyr::compute(bq_mtcars, temporary = FALSE, name = name)
  expect_equal(dim(dplyr::collect(perm)), c(11, 11))
  defer(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
})

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

  x <- dplyr::collect(bq1)
  expect_s3_class(x, "tbl")

  bq5 <- dplyr::tbl(con, dplyr::sql("SELECT * FROM mtcars"))
  expect_false(op_can_download(bq5))
  expect_false(op_can_download(head(bq5)))

  DBI::dbExecute(con, 'CREATE VIEW mtcars2 AS SELECT * FROM basedata.mtcars')
  defer(DBI::dbExecute(con, 'DROP VIEW mtcars2'))
  bq6 <- dplyr::tbl(con, "mtcars2")
  expect_false(op_can_download(bq6))

  # INFORMATION_SCHEMA.TABLES errors when we attempt to get metadata
  bq7 <- dplyr::tbl(con, "INFORMATION_SCHEMA.PARTITIONS")
  expect_false(op_can_download(bq7))
  expect_false(op_can_download(head(bq7)))
})

test_that("casting uses bigquery types", {
  skip_if_not_installed("dbplyr")

  sql <- dbplyr::lazy_frame(x = "1") %>%
    dplyr::mutate(y = as.integer(x), z = as.numeric(x)) %>%
    dbplyr::sql_build(simulate_bigrquery())

  expect_equal(sql$select[[2]], 'SAFE_CAST(`x` AS INT64)')
  expect_equal(sql$select[[3]], 'SAFE_CAST(`x` AS FLOAT64)')
})

test_that("%||% translates to IFNULL", {
  skip_if_not_installed("dbplyr")

  sql <- dbplyr::lazy_frame(x = 1L) %>%
    dplyr::mutate(y = x %||% 2L) %>%
    dbplyr::sql_build(simulate_bigrquery())

  expect_equal(sql$select[[2]], 'IFNULL(`x`, 2)')
})

test_that("suffixes use _", {
  skip_if_not_installed("dbplyr", "1.99")

  expect_equal(dbplyr::sql_join_suffix(simulate_bigrquery()), c("_x", "_y"))
})

test_that("all BigQuery tbls share the same src", {
  con1 <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project()
  )
  con2 <- DBI::dbConnect(
    bigquery(),
    project = "publicdata",
    billing = bq_test_project()
  )

  tbl1 <- dplyr::tbl(con1, "basedata.mtcars", vars = "x")
  tbl2 <- dplyr::tbl(con2, "publicdata.samples.natality", vars = "x")
  expect_true(dplyr::same_src(tbl1, tbl2))
  expect_false(dplyr::same_src(tbl1, mtcars))
})

test_that("runif is correctly translated", {
  expect_equal(
    dbplyr::translate_sql(runif(n()), con = simulate_bigrquery()),
    dbplyr::sql("RAND()")
  )
})

test_that("string functions correctly", {
  con <- simulate_bigrquery()

  expect_snapshot({
    dbplyr::translate_sql(grepl("a.c", x), con = con)
    dbplyr::translate_sql(gsub("a.c", "", x), con = con)
  })
})

test_that("median is correctly translated", {
  expect_equal(
    dbplyr::translate_sql(median(x), con = simulate_bigrquery(), window = FALSE),
    dbplyr::sql("APPROX_QUANTILES(`x`, 2)[SAFE_ORDINAL(2)]")
  )
})

test_that("can correctly print a lazy query", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  bq_mtcars <- dplyr::tbl(con, "mtcars")

  # not a snapshot test because column order can vary
  expect_no_error(
    expect_output(
      print(bq_mtcars)
    )
  )
})
