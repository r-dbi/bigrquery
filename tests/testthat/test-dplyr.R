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

  # If if has three or more periods (#611)
  y <- dplyr::tbl(
    con_us,
    dplyr::sql("SELECT c.fips_code, c.country_name FROM country_code_iso c")
  )
  expect_s3_class(y, "tbl")
})

test_that("can work with nested table identifier", {
  con1 <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    billing = bq_test_project()
  )
  # As far as I can tell from the BigQuery API there's no way to provide
  # a default project; you can either provide a default dataset + project or
  # nothing
  table_name <- I("bigquery-public-data.utility_us.country_code_iso")
  expect_no_error(dplyr::collect(
    head(dplyr::tbl(con1, table_name)),
    api = "arrow"
  ))

  con2 <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "utility_us",
    billing = bq_test_project(),
  )
  expect_no_error(dplyr::collect(
    head(dplyr::tbl(con2, "country_code_iso")),
    api = "arrow"
  ))
})

test_that("can copy_to", {
  ds <- bq_test_shared_dataset()
  con <- DBI::dbConnect(ds)

  expect_snapshot(dplyr::copy_to(con, mtcars), error = TRUE)
  bq_mtcars <- suppressMessages(dplyr::copy_to(con, mtcars, temporary = FALSE))

  expect_s3_class(bq_mtcars, "tbl_BigQueryConnection")
})

test_that("can collect and compute (no dataset)", {
  con <- DBI::dbConnect(bigquery(), project = bq_test_project())
  bq_mtcars <- dplyr::tbl(con, "basedata.mtcars") %>% dplyr::filter(cyl == 4)

  # collect
  x <- dplyr::collect(bq_mtcars, api = "arrow")
  expect_equal(dim(x), c(11, 11))

  # compute: temporary
  temp <- dplyr::compute(bq_mtcars)
  expect_equal(dim(dplyr::collect(temp, api = "arrow")), c(11, 11))

  # compute: persistent
  name <- paste0("basedata.", random_name())
  if (packageVersion("dbplyr") >= "2.4.0.9000") {
    name <- I(name)
  }
  perm <- dplyr::compute(bq_mtcars, name = name, temporary = FALSE)
  defer(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
  expect_equal(dim(dplyr::collect(perm, api = "arrow")), c(11, 11))
})

test_that("can collect and compute (with dataset)", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )
  bq_mtcars <- dplyr::tbl(con, "mtcars") %>% dplyr::filter(cyl == 4)

  # collect
  x <- dplyr::collect(bq_mtcars, api = "arrow")
  expect_equal(dim(x), c(11, 11))

  # compute: temporary
  temp <- dplyr::compute(bq_mtcars)
  expect_equal(dim(dplyr::collect(temp, api = "arrow")), c(11, 11))

  # compute: persistent
  name <- random_name()
  perm <- dplyr::compute(bq_mtcars, temporary = FALSE, name = name)
  expect_equal(dim(dplyr::collect(perm, api = "arrow")), c(11, 11))
  defer(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
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

  tbl1 <- dplyr::tbl(con1, I("basedata.mtcars"), vars = "x")
  tbl2 <- dplyr::tbl(con2, I("publicdata.samples.natality"), vars = "x")
  expect_true(dplyr::same_src(tbl1, tbl2))
  expect_false(dplyr::same_src(tbl1, mtcars))
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
      suppressMessages(print(bq_mtcars))
    )
  )
})
