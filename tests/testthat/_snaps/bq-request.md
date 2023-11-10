# error call is forwarded all the way down

    Code
      bq_job_meta("a.b.c")
    Condition
      Error in `bq_get()`:
      ! Invalid value for location: c is not a valid value [invalid]

