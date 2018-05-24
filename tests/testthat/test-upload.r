context("job-upload.R")

test_that("date/times can be round-tripped", {
  ds <- bq_test_dataset()

  df1 <- data.frame(
    x = as.Date("2018-01-01"),
    y = as.POSIXct(as.Date("2018-01-01"))
  )
  attr(df1$y, "tzone") <- "UTC"
  df1$z <- as.POSIXlt(df1$y)

  job <- insert_upload_job(ds$project, ds$dataset, "x", df1)
  wait_for(job)

  df2 <- list_tabledata(ds$project, ds$dataset, "x")
  expect_equal(df1, df2)
})

