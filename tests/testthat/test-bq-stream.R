context("bq-stream.R")

test_that("can stream data.frame into bigquery table", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "cars")

  bq_table_create(tb, cars)
  bq_table_stream(tb, cars)
  job <- bq_perform_query("SELECT COUNT(*) cnt FROM cars")
  job <- bq_job_wait(job)
  streamed.rows <- bq_job_table(job)

  expect_equal(nrow(streamed.rows), nrow(cars))
})
