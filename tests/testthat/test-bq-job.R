test_that("can control chattiness of bq_job_wait", {
  job <- bq_perform_query("SELECT 1 + 1", bq_test_project())

  expect_message(bq_job_wait(job, quiet = TRUE), NA)
  expect_message(bq_job_wait(job, quiet = FALSE), "Complete")
  expect_message(bq_job_wait(job, quiet = NA), if (interactive()) "Complete" else NA)
})

test_that("informative errors on failure", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "df")

  fields <- bq_fields(list(bq_field("x", "integer"), bq_field("y", "string")))
  bq_mtcars <- bq_table_create(tb, fields = fields)

  verify_output(test_path("bq-job-errors.txt"), {
    "One error"
    bq_dataset_query(ds, "SELECT 1 +", quiet = TRUE)

    "Multiple erros"
    bq_table_upload(tb, data.frame(x = 1, y = 1:5), quiet = TRUE)
  })
})
