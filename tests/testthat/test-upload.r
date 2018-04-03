context("upload")

test_that("date/times can be round-tripped", {
  skip_if_no_auth()

  df1 <- data.frame(
    x = Sys.Date(),
    y = Sys.time()
  )
  df1$z <- as.POSIXlt(df1$y)

  job <- insert_upload_job(bq_test_project(), "test", "x", df1)
  wait_for(job)
  on.exit(delete_table(bq_test_project(), "test", "x"))

  df2 <- list_tabledata(bq_test_project(), "test", "x")
  expect_equal(df1, df2)
})
