# now <- Sys.time()
tbl <- data.frame(dbl=pi, psx=Sys.time())
attr(tbl$psx, "tzone") <- "America/New_York"

test_that("'digits' and 'digits.secs' preserve numerical precision", {

  # ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  # known issue: jsonlite_2.0.0 toJSON()/stream_out() treat default
  # digits differently when not provided; because of this, setting
  # nothing results in an assumed `digits=5` despite expectation and
  # documentation


  withr::with_options(
    list(bigrquery.digits = NULL, bigrquery.digits.secs = NULL,
         bigrquery.jsonlite.toJSON = NULL),
    bq_table_upload(ds, tbl)
  )
  tbl2 <- bq_table_download(ds)
  bq_table_delete(ds)

  expect_equal(log10(abs(tbl2$dbl - tbl$dbl)), -5, tolerance = 1)
  expect_equal(log10(abs(as.numeric(tbl2$psx - tbl$psx, units = "secs"))), 0, tolerance = 0.5)


  # bigrquery.digits is inferred for empty bigrquery.jsonlite.toJSON
  withr::with_options(
    list(bigrquery.digits = 22, bigrquery.digits.secs = 6,
         bigrquery.jsonlite.toJSON = list()),
    bq_table_upload(ds, tbl)
  )
  tbl3 <- bq_table_download(ds)
  bq_table_delete(ds)

  expect_true(log10(abs(tbl3$dbl - tbl$dbl)) < -21)
  expect_equal(log10(abs(as.numeric(tbl3$psx - tbl$psx, units = "secs"))), -6, tolerance = 0.5)


  # bigrquery.jsonlite.toJSON$digits overrides bigrquery.digits
  withr::with_options(
    list(bigrquery.digits = 4, bigrquery.digits.secs = 6,
         bigrquery.jsonlite.toJSON = list(digits = NA)),
    bq_table_upload(ds, tbl)
  )
  tbl4 <- bq_table_download(ds)
  bq_table_delete(ds)

  expect_true(log10(abs(tbl3$dbl - tbl$dbl)) < -21)
  expect_equal(log10(abs(as.numeric(tbl4$psx - tbl$psx, units = "secs"))), -6, tolerance = 0.5)

})

test_that("'tzone' unset and different values handled correctly", {

  # the bug only produced problems when the system TZ has a non-zero
  # offset (i.e., not UTC)
  skip_if(Sys.timezone() %in% "UTC")

  # ds <- bq_table(bq_test_project(), "basedata", "datatypes")
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
