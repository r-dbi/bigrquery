test_that("casting uses bigquery types", {
  skip_if_not_installed("dbplyr")
  con <- simulate_bigrquery()

  expect_equal(
    dbplyr::translate_sql(as.integer(x), con = con),
    dbplyr::sql("SAFE_CAST(`x` AS INT64)")
  )
  expect_equal(
    dbplyr::translate_sql(as.numeric(x), con = con),
    dbplyr::sql("SAFE_CAST(`x` AS FLOAT64)")
  )
})

test_that("%||% translates to IFNULL", {
  skip_if_not_installed("dbplyr")

  expect_equal(
    dbplyr::translate_sql(x %||% 2L, con = simulate_bigrquery()),
    dbplyr::sql("IFNULL(`x`, 2)")
  )
})

test_that("suffixes use _", {
  skip_if_not_installed("dbplyr", "1.99")

  expect_equal(dbplyr::sql_join_suffix(simulate_bigrquery()), c("_x", "_y"))
})

test_that("runif is correctly translated", {
  skip_if_not_installed("dbplyr", "2.4.0")
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
    dbplyr::translate_sql(
      median(x),
      con = simulate_bigrquery(),
      window = FALSE
    ),
    dbplyr::sql("APPROX_QUANTILES(`x`, 2)[SAFE_ORDINAL(2)]")
  )
})

test_that("lubridate rounding functions translate to correct sql", {
  skip_if_not_installed("dbplyr")
  con <- simulate_bigrquery()

  expect_equal(
    dbplyr::translate_sql(floor_date(x), con = con),
    dbplyr::sql("DATE_TRUNC(`x`, SECOND)")
  )
  expect_equal(
    dbplyr::translate_sql(floor_date(x, "months"), con = con),
    dbplyr::sql("DATE_TRUNC(`x`, MONTH)")
  )
  expect_snapshot({
    dbplyr::translate_sql(ceiling_date(x, "hour"), con = con)
    dbplyr::translate_sql(ceiling_date(x, "month"), con = con)
    dbplyr::translate_sql(round_date(x, "day"), con = con)
  })
  expect_snapshot(
    dbplyr::translate_sql(floor_date(x, "fortnight"), con = con),
    error = TRUE
  )
})

test_that("ceiling_date supports change_on_boundary", {
  con <- simulate_bigrquery()
  expect_equal(
    dbplyr::translate_sql(
      ceiling_date(x, "day", change_on_boundary = FALSE),
      con = con
    ),
    dbplyr::sql(
      "IF(DATE_TRUNC(`x`, DAY) = `x`, `x`, DATE_ADD(DATE_TRUNC(`x`, DAY), INTERVAL 1 DAY))"
    )
  )
  expect_equal(
    dbplyr::translate_sql(
      ceiling_date(x, "day", change_on_boundary = NULL),
      con = con
    ),
    dbplyr::translate_sql(ceiling_date(x, "day"), con = con)
  )
  expect_equal(
    dbplyr::translate_sql(ceiling_date(x, "day"), con = con),
    dbplyr::sql(
      "IF(TYPEOF(`x`) != 'DATE' AND DATE_TRUNC(`x`, DAY) = `x`, `x`, DATE_ADD(DATE_TRUNC(`x`, DAY), INTERVAL 1 DAY))"
    )
  )
  expect_equal(
    dbplyr::translate_sql(
      ceiling_date(x, "day", change_on_boundary = TRUE),
      con = con
    ),
    dbplyr::sql("DATE_ADD(DATE_TRUNC(`x`, DAY), INTERVAL 1 DAY)")
  )
  expect_equal(
    dbplyr::translate_sql(
      ceiling_date(x, "month", change_on_boundary = TRUE),
      con = con
    ),
    dbplyr::sql(
      "DATE_TRUNC(DATE_ADD(DATE_TRUNC(`x`, MONTH), INTERVAL 31 DAY), MONTH)"
    )
  )
})

test_that("round_date preserves compound expressions", {
  sql <- dbplyr::translate_sql(
    round_date(x + 1L, "day"),
    con = simulate_bigrquery()
  )
  expect_match(sql, "IF((`x` + 1) - ", fixed = TRUE)
  expect_match(sql, " - (`x` + 1), ", fixed = TRUE)
})

test_that("clock functions translate to correct sql", {
  skip_if_not_installed("dbplyr")
  con <- simulate_bigrquery()

  expect_equal(
    dbplyr::translate_sql(get_year(x), con = con),
    dbplyr::sql("EXTRACT(YEAR FROM `x`)")
  )
  expect_equal(
    dbplyr::translate_sql(get_month(x), con = con),
    dbplyr::sql("EXTRACT(MONTH FROM `x`)")
  )
  expect_equal(
    dbplyr::translate_sql(get_day(x), con = con),
    dbplyr::sql("EXTRACT(DAY FROM `x`)")
  )

  expect_equal(
    dbplyr::translate_sql(add_years(x, 1L), con = con),
    dbplyr::sql("DATE_ADD(CAST(`x` AS DATE), INTERVAL 1 YEAR)")
  )
  expect_equal(
    dbplyr::translate_sql(add_days(x, 1L), con = con),
    dbplyr::sql("DATE_ADD(CAST(`x` AS DATE), INTERVAL 1 DAY)")
  )

  expect_equal(
    dbplyr::translate_sql(date_build(y, m, d), con = con),
    dbplyr::sql("DATE(`y`, `m`, `d`)")
  )
  expect_equal(
    dbplyr::translate_sql(date_build(y), con = con),
    dbplyr::sql("DATE(`y`, 1, 1)")
  )

  expect_equal(
    dbplyr::translate_sql(date_count_between(start, end, "DAY"), con = con),
    dbplyr::sql("DATE_DIFF(CAST(`end` AS DATE), CAST(`start` AS DATE), DAY)")
  )
  expect_equal(
    dbplyr::translate_sql(
      difftime(time1 = time1, time2 = time2, units = "DAY"),
      con = con
    ),
    dbplyr::sql("DATE_DIFF(CAST(`time1` AS DATE), CAST(`time2` AS DATE), DAY)")
  )
})
