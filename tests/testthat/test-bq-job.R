test_that("can control chattiness of bq_job_wait", {
  job <- bq_perform_query("SELECT 1 + 1", bq_test_project())

  expect_snapshot({
    bq_job_wait(job, quiet = TRUE)
    bq_job_wait(job, quiet = FALSE)
  })
})

test_that("informative errors on failure", {
  withr::local_options(cli.progress_show_after = 10)
  ds <- bq_test_dataset()

  tb <- bq_test_table()
  bq_table_create(tb, fields = list(bq_field("x", "integer"), bq_field("y", "string")))

  expect_snapshot(
    {
      "One error"
      bq_dataset_query(ds, "SELECT 1 +")

      "Multiple errors"
      bq_table_upload(tb, data.frame(x = "x", y = 1:5))
    },
    error = TRUE,
    transform = function(x) gsub("Job (.*?) failed", "Job <bq_job> failed", x)
  )
})
