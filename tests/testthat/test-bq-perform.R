context("test-bq-perform.R")

test_that("bq_perform_upload creates job that succeeds", {
  ds <- bq_test_dataset()
  bq_mtcars <- bq_table(ds, "mtcars")

  job <- bq_perform_upload(bq_mtcars, mtcars)
  expect_s3_class(job, "bq_job")
  expect_message(bq_job_wait(job), "Input")
  expect_message(bq_job_wait(job), "Output")

  expect_true(bq_table_exists(bq_mtcars))
})

test_that("bq_perform_copy creates job that succeeds", {
  ds <- bq_test_dataset()

  src <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  dst <- bq_table(ds, "my_moon")

  job <- bq_perform_copy(src, dst)
  expect_s3_class(job, "bq_job")

  # Doesn't return any statistics to show
  expect_message(bq_job_wait(job), "Complete")

  expect_true(bq_table_exists(dst))
})

# Queries -----------------------------------------------------------------

test_that("bq_perform_query creates job that succeeds", {
  ds <- as_bq_dataset("bigquery-public-data.moon_phases")
  job <- bq_perform_query(
    "SELECT count(*) FROM moon_phases",
    billing = bq_test_project(),
    default_dataset = ds
  )

  expect_s3_class(job, "bq_job")
  expect_message(bq_job_wait(job), "Billed")

  job_tb <- bq_job_table(job)
  expect_true(bq_table_exists(job_tb))
})

test_that("can supply parameters", {
  ds <- as_bq_dataset("bigquery-public-data.moon_phases")
  job <- bq_perform_query(
    "SELECT * FROM moon_phases WHERE peak_datetime = @date",
    parameters = list(date = bq_param("1889-07-28 00:00:00", "DATETIME")),
    billing = bq_test_project(),
    default_dataset = ds
  )
  job <- bq_job_wait(job)
  job_tb <- bq_job_table(job)

  df <- bq_table_download(job_tb)
  expect_equal(nrow(df), 1)
  expect_equal(df$phase, "New Moon")
})
