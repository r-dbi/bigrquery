# can control chattiness of bq_job_wait

    Code
      bq_job_wait(job, quiet = TRUE)
      bq_job_wait(job, quiet = FALSE)
    Message
      Complete
      Billed: 0 B
    Code
      bq_job_wait(job, quiet = NA)

# informative errors on failure

    Code
      # One error
      bq_dataset_query(ds, "SELECT 1 +", quiet = TRUE)
    Condition
      Error in `bq_job_wait()`:
      ! Job '' failed
      x Syntax error: Unexpected end of script at [1:11] [invalidQuery]
    Code
      # Multiple erros
      bq_table_upload(tb, data.frame(x = 1, y = 1:5), quiet = TRUE)

