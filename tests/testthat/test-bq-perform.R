context("test-bq-perform.R")

test_that("bq_perform_upload creates job that succeeds", {
  ds <- bq_test_dataset()
  bq_mtcars <- bq_table(ds, "mtcars")

  job <- bq_perform_upload(bq_mtcars, mtcars)
  expect_s3_class(job, "bq_job")
  expect_message(bq_job_wait(job, quiet = FALSE), "Input")
  expect_message(bq_job_wait(job, quiet = FALSE), "Output")

  expect_true(bq_table_exists(bq_mtcars))
})

test_that("bq_perform_copy creates job that succeeds", {
  ds <- bq_test_dataset()

  src <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  dst <- bq_table(ds, "my_moon")

  job <- bq_perform_copy(src, dst)
  expect_s3_class(job, "bq_job")

  # Doesn't return any statistics to show
  expect_message(bq_job_wait(job, quiet = FALSE), "Complete")

  expect_true(bq_table_exists(dst))
})


# Load / extract ----------------------------------------------------------

test_that("can round trip extract + load", {
  ds_public <- bq_dataset("bigquery-public-data", "moon_phases")
  ds_mine <- bq_test_dataset()

  tb <- bq_dataset_query(ds_public,
    query = "SELECT COUNT(*) as count FROM moon_phases",
    billing = bq_test_project()
  )

  tmp <- gs_test_object()
  # on.exit(gs_object_delete(tmp))

  job <- bq_perform_extract(tb, tmp)
  bq_job_wait(job)

  tb_ks <- bq_table(ds_mine, "natality_ks")
  job <- bq_perform_load(tb_ks, tmp)
  bq_job_wait(job)

  df <- bq_table_download(tb_ks)
  expect_equal(nrow(df), 1)
  expect_named(df, "count")
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
  expect_message(bq_job_wait(job, quiet = FALSE), "Billed")

  job_tb <- bq_job_table(job)
  expect_true(bq_table_exists(job_tb))
})

test_that("can supply parameters", {
  ds <- as_bq_dataset("bigquery-public-data.moon_phases")

  job <- bq_perform_query(
    "SELECT * FROM moon_phases WHERE peak_datetime = CAST(@date AS DATETIME)",
    parameters = list(date = as.Date("1889-07-28 00:00:00")),
    billing = bq_test_project(),
    default_dataset = ds
  )
  job <- bq_job_wait(job)
  job_tb <- bq_job_table(job)

  df <- bq_table_download(job_tb)
  expect_equal(nrow(df), 1)
  expect_equal(df$phase, "New Moon")

})

test_that("can supply parameters as array for IN statement", {

  query_template <- "#StandardSql
  SELECT values FROM UNNEST(@values) values"

  job <- bq_perform_query(
    query_template,
    parameters = list(
      values = bq_param_array(c("a", "b"))
    ),
    billing = bq_test_project(),
    default_dataset = bq_test_dataset()
  )
  job <- bq_job_wait(job)
  job_tb <- bq_job_table(job)
  df <- bq_table_download(job_tb)

  expect_equal(
    setdiff(df$values, c("a", "b")),
    0L,
    label = "Query gets expected number of half moons"
  )

  # Same works with scalar and vector
  job <- bq_perform_query(
    query_template,
    parameters = list(
      values = c("a", "b")
    ),
    billing = bq_test_project(),
    default_dataset = bq_test_dataset()
  )
  job <- bq_job_wait(job)
  job_tb <- bq_job_table(job)
  df <- bq_table_download(job_tb)

  expect_equal(
    setdiff(df$values, c("a", "b")),
    0L,
    label = "Query counts expected number of records"
  )

  # Try the same but with scalar value for the array param
  job <- bq_perform_query(
    query_template,
    parameters = list(
      values = bq_param_array(c("c"))
    ),
    billing = bq_test_project(),
    default_dataset = bq_test_dataset()
  )
  job <- bq_job_wait(job)
  job_tb <- bq_job_table(job)
  df <- bq_table_download(job_tb)

  expect_equal(
    df$values,
    c("c"),
    label = "Query counts expected number records"
  )

})
