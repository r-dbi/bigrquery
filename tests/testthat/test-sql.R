context("sql")

test_that("casting uses bigquery types", {
  skip_if_not_installed("dbplyr")

  sql <- dbplyr::lazy_frame(x = "1", src = simulate_bigrquery()) %>%
    dplyr::mutate(y = as.integer(x), z = as.numeric(x)) %>%
    dbplyr::sql_build()

  expect_equal(sql$select[[2]], 'CAST(`x` AS INT64) AS `y`')
  expect_equal(sql$select[[3]], 'CAST(`x` AS FLOAT64) AS `z`')
})
