tbl <- data.frame(psx = trunc.Date(Sys.time(), units = "secs") + 0.123456)
attr(tbl$psx, "tzone") <- "America/New_York"

test_that("'POSIXt' preserves microsecond precision", {

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  bq_table_upload(ds, tbl)
  tbl2 <- bq_table_download(ds)

  expect_true(abs(as.numeric(tbl2$psx - tbl$psx, units = "secs")) < 1e-6)

})

test_that("'tzone' unset and different values handled correctly", {

  # the bug only produced problems when the system TZ has a non-zero
  # offset (i.e., not UTC)
  withr::local_envvar(TZ = "America/New_York")

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))


  bq_table_upload(ds, transform(tbl, psx = `attr<-`(psx, "tzone", NULL)))
  tbl5 <- bq_table_download(ds)
  bq_table_delete(ds)
  expect_true(abs(as.numeric(tbl5$psx - tbl$psx, units = "hours")) < 1)


  bq_table_upload(ds, transform(tbl, psx = `attr<-`(psx, "tzone", "UTC")))
  tbl6 <- bq_table_download(ds)
  bq_table_delete(ds)
  expect_true(abs(as.numeric(tbl6$psx - tbl$psx, units = "hours")) < 1)


  bq_table_upload(ds, transform(tbl, psx = `attr<-`(psx, "tzone", "America/New_York")))
  tbl7 <- bq_table_download(ds)
  bq_table_delete(ds)
  expect_true(abs(as.numeric(tbl7$psx - tbl$psx, units = "hours")) < 1)

})
