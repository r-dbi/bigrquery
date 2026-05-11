# dbConnect() validates labels

    Code
      dbConnect(bigquery(), project = bq_test_project(), labels = "oops")
    Condition
      Error in `dbConnect()`:
      ! `labels` must be a named list or `NULL`, not the string "oops".

