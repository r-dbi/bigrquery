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
  con <- DBI::dbConnect(
    bigquery(),
    project = bq_test_project(),
    dataset = "basedata"
  )

  bq_mtcars <- dplyr::tbl(con, "mtcars")
  bq_mtcars <- bq_mtcars %>% dplyr::filter(cyl == 4)

  name <- random_name()
  x <- dplyr::compute(bq_mtcars, name, temporary = FALSE)
  on.exit(DBI::dbRemoveTable(con, name))

  expect_true(DBI::dbExistsTable(con, name))
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
