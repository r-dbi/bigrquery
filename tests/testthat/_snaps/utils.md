# bq_check_namespace() works

    Code
      bq_check_namespace("invalid package name", "FIELD_TYPE")
    Condition
      Error in `bq_check_namespace()`:
      ! The package "invalid package name" is required to parse BigQuery 'FIELD_TYPE' fields.

# check_labels() errors on invalid inputs

    Code
      check_labels(c(env = "prod"))
    Condition
      Error:
      ! `c(env = "prod")` must be a named list or `NULL`, not the string "prod".
    Code
      check_labels(list("no-name"))
    Condition
      Error:
      ! `list("no-name")` must be a named list or `NULL`, not a list.
    Code
      check_labels(list(env = 1))
    Condition
      Error:
      ! `list(env = 1)` must be a named list of strings, not a list.

