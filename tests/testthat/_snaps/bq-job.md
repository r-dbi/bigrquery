# can control chattiness of bq_job_wait

    Code
      bq_job_wait(job, quiet = TRUE)
      bq_job_wait(job, quiet = FALSE)
    Message
      Job complete
      Billed: 0 B

# informative errors on failure

    Code
      # One error
      bq_dataset_query(ds, "SELECT 1 +")
    Condition
      Error in `bq_dataset_query()`:
      ! Job <bq_job> failed
      x Syntax error: Unexpected end of script at [1:11] [invalidQuery]
    Code
      # Multiple errors
      bq_table_upload(tb, data.frame(x = "x", y = 1:5))
    Condition
      Error in `bq_table_upload()`:
      ! Job <bq_job> failed
      x Error while reading data, error message: JSON processing encountered too many errors, giving up. Rows: 1; errors: 1; max bad: 0; error percent: 0 [invalid]
      x Error while reading data, error message: JSON parsing error in row starting at position 0: Could not convert value 'string_value: "x"' to integer. Field: x; Value: x [invalid]

