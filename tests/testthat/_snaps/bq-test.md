# error if env var not set

    Code
      bq_test_project()
    Condition
      Error:
      ! To run bigrquery tests you must have BIGQUERY_TEST_PROJECT envvar set to name of project which has billing set up and to which you have write access
    Code
      gs_test_bucket()
    Condition
      Error:
      ! To run bigrquery extract/load tests you must have BIGQUERY_TEST_BUCKET set to name of the bucket where `bq_test_project()` has write acess

