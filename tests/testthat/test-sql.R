context("sql")

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
