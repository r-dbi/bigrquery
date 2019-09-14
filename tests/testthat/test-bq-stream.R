context("bq-stream.R")

test_that("can stream data.frame into bigquery table", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "cars")

  bq_table_create(tb, cars)
  expect_null(bq_table_stream(tb, cars))

  streamed.rows.tb <- bigrquery::bq_dataset_query(
    x = bq_dataset(tb$project, tb$dataset),
    query = "SELECT COUNT(*) cnt FROM cars",
    billing = bq_test_project(),
  )

  streamed.rows <- bq_table_download(streamed.rows.tb)
  expect_equal(streamed.rows$cnt, nrow(cars))

  # Check that streaming of data with wrong schema returns erros
  cars.wrong.schema <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
  errors <- bq_table_stream(tb, cars.wrong.schema)
  expect_length(errors, 3L)
  expect_match(toJSON(errors), "index.*reason.*location")
})
