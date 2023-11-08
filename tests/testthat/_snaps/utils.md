# bq_check_namespace() works

    Code
      bq_check_namespace("invalid package name", "FIELD_TYPE")
    Condition
      Error in `bq_check_namespace()`:
      ! Package 'invalid package name' must be installed to load BigQuery field with type 'FIELD_TYPE'

