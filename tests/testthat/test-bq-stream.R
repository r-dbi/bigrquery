context("bq-stream.R")

test_that("can stream data.frame into bigquery table", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "cars")

  bq_table_create(tb, cars)
  bq_table_stream(tb, cars)

  streamed.rows.tb <- bigrquery::bq_dataset_query(
    x = bq_dataset(tb$project, tb$dataset),
    query = "SELECT COUNT(*) cnt FROM cars",
    billing = bq_test_project(),
  )

  streamed.rows <- bq_table_download(streamed.rows.tb)

  expect_equal(streamed.rows$cnt, nrow(cars))
})
