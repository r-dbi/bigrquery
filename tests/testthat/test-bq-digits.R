tbl <- data.frame(dbl = pi)

test_that("'digits' and preserves numerical precision", {

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  # known issue: jsonlite_2.0.0 toJSON()/stream_out() treat default
  # digits differently when not provided; because of this, setting
  # nothing results in an assumed `digits=5` despite expectation and
  # documentation


  withr::with_options(
    list(bigrquery.digits = NULL, bigrquery.jsonlite.toJSON = NULL),
    bq_table_upload(ds, tbl)
  )
  tbl2 <- bq_table_download(ds)
  bq_table_delete(ds)

  expect_equal(log10(abs(tbl2$dbl - tbl$dbl)), -5, tolerance = 1)


  # bigrquery.digits is inferred for empty bigrquery.jsonlite.toJSON
  withr::with_options(
    list(bigrquery.digits = 22, bigrquery.jsonlite.toJSON = list()),
    bq_table_upload(ds, tbl)
  )
  tbl3 <- bq_table_download(ds)
  bq_table_delete(ds)

  expect_true(log10(abs(tbl3$dbl - tbl$dbl)) < -21)


  # bigrquery.jsonlite.toJSON$digits overrides bigrquery.digits
  withr::with_options(
    list(bigrquery.digits = 4, bigrquery.jsonlite.toJSON = list(digits = NA)),
    bq_table_upload(ds, tbl)
  )
  tbl4 <- bq_table_download(ds)
  bq_table_delete(ds)

  expect_true(log10(abs(tbl3$dbl - tbl$dbl)) < -21)

})
