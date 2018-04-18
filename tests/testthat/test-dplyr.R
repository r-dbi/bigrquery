context("dplyr.R")

test_that("can work with literal SQL", {
  con_us <- DBI::dbConnect(
    bigquery(),
    project = "bigquery-public-data",
    dataset = "utility_us",
    billing = bq_test_project()
  )

  x <- dplyr::tbl(con_us, dplyr::sql("SELECT * FROM country_code_iso"))
  expect_true("fips_code" %in% dbplyr::op_vars(x))
})

test_that("can compute queries", {
  skip_if_not_installed("dbplyr", "1.2.0.9001")

  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  bq_mtcars <- dplyr::tbl(con, "mtcars")
  bq_mtcars <- bq_mtcars %>% dplyr::filter(cyl == 4)

  # Temporary table
  temp <- dplyr::compute(bq_mtcars)

  # Persistently
  name <- random_name()
  temp <- dplyr::compute(bq_mtcars, temporary = FALSE, name = name)
  on.exit(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
})

test_that("collect can identify  directly download tables", {
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  bq1 <- dplyr::tbl(con, "mtcars")
  expect_true(op_can_download(bq1$ops))
  expect_equal(op_rows(bq1$ops), Inf)

  bq2 <- head(bq1, 4)
  expect_true(op_can_download(bq2$ops))
  expect_equal(op_rows(bq2$ops), 4)

  bq3 <- head(bq2, 2)
  expect_true(op_can_download(bq3$ops))
  expect_equal(op_rows(bq3$ops), 2)
})

test_that("casting uses bigquery types", {
  skip_if_not_installed("dbplyr")

  sql <- dbplyr::lazy_frame(x = "1") %>%
    dplyr::mutate(y = as.integer(x), z = as.numeric(x)) %>%
    dbplyr::sql_build(simulate_bigrquery())

  expect_equal(sql$select[[2]], 'SAFE_CAST(`x` AS INT64) AS `y`')
  expect_equal(sql$select[[3]], 'SAFE_CAST(`x` AS FLOAT64) AS `z`')
})

test_that("%||% translates to IFNULL", {
  skip_if_not_installed("dbplyr")

  sql <- dbplyr::lazy_frame(x = 1L) %>%
    dplyr::mutate(y = x %||% 2L) %>%
    dbplyr::sql_build(simulate_bigrquery())

  expect_equal(sql$select[[2]], 'IFNULL(`x`, 2) AS `y`')
})
