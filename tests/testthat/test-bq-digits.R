tbl <- data.frame(dbl = pi)

test_that("'digits' and preserves numerical precision", {

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  bq_table_upload(ds, tbl)
  tbl2 <- bq_table_download(ds)

  expect_equal(tbl$dbl, tbl2$dbl, tolerance = 1e-15)

})
