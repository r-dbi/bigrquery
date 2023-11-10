# useful error if can't auth

    Code
      bq_auth()
    Condition
      Error in `bq_auth()`:
      ! Can't get Google credentials.
      i Try calling `bq_auth()` directly with necessary specifics.

# bq_auth_configure checks its inputs

    Code
      bq_auth_configure(1, 1)
    Condition
      Error in `bq_auth_configure()`:
      ! Exactly one of `client` or `path` must be supplied.
    Code
      bq_auth_configure(client = 1)
    Condition
      Error in `bq_auth_configure()`:
      ! `client` must be a gargle OAuth client or `NULL`, not the number 1.
    Code
      bq_auth_configure(path = 1)
    Condition
      Error in `bq_auth_configure()`:
      ! `path` must be a single string, not the number 1.

