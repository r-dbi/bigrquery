test_that("'POSIXt' preserves microsecond precision", {
  tbl <- data.frame(psx = trunc.Date(Sys.time(), units = "secs") + 0.123456)
  attr(tbl$psx, "tzone") <- "UTC"

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  bq_table_upload(ds, tbl)
  tbl2 <- bq_table_download(ds)

  expect_equal(tbl$psx, tbl2$psx)
})

test_that("'tzone' unset and different values handled correctly", {
  # the bug only produced problems when the system TZ has a non-zero
  # offset (i.e., not UTC)
  withr::local_envvar(TZ = "America/New_York")

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  tbl1in <- tibble(psx = trunc.Date(Sys.time(), units = "secs") + 0.123456)
  attr(tbl1in$psx, "tzone") <- NULL
  bq_table_upload(ds, tbl1in)
  tbl1out <- bq_table_download(ds)
  bq_table_delete(ds)
  attr(tbl1out$psx, "tzone") <- NULL
  expect_equal(tbl1in$psx, tbl1out$psx)
  expect_true(abs(as.numeric(tbl5$psx - tbl$psx, units = "hours")) < 1)

  tbl2in <- tibble(psx = trunc.Date(Sys.time(), units = "secs") + 0.123456)
  attr(tbl2in$psx, "tzone") <- "UTC"
  bq_table_upload(ds, tbl2in)
  tbl2out <- bq_table_download(ds)
  bq_table_delete(ds)
  expect_equal(tbl2in$psx, tbl2out$psx)

  tbl3in <- tibble(psx = trunc.Date(Sys.time(), units = "secs") + 0.123456)
  attr(tbl3in$psx, "tzone") <- "America/New_York"
  bq_table_upload(ds, tbl3in)
  tbl3out <- bq_table_download(ds)
  bq_table_delete(ds)
  attr(tbl3out$psx, "tzone") <- attr(tbl3in$psx, "tzone")
  expect_equal(tbl3in$psx, tbl3out$psx)
})
