# can make table with dataset

    Code
      bq_table(ds, 1)
    Condition
      Error in `bq_table()`:
      ! `dataset` must be a single string, not the number 1.

# objects have helpful print methods

    Code
      as_bq_job("x.y.US")
    Output
      <bq_job> x.y.US
    Code
      as_bq_dataset("x.y")
    Output
      <bq_dataset> x.y
    Code
      as_bq_table("x.y.z")
    Output
      <bq_table> x.y.z

# useful error for non-strings

    Code
      as_bq_job(1)
    Condition
      Error:
      ! `1` must be a string, list, or `bq_job()`.
    Code
      as_bq_dataset(1)
    Condition
      Error:
      ! `1` must be a string, list, or `bq_dataset()`.
    Code
      as_bq_table(1)
    Condition
      Error:
      ! `1` must be a string, list, or `bq_table()`.

# string coercion error on invalid number of components

    Code
      as_bq_table("x")
    Condition
      Error:
      ! When `"x"` is a string, it must contain 3 components separted by ".".
    Code
      as_bq_table("a.b.c.d")
    Condition
      Error:
      ! When `"a.b.c.d"` is a string, it must contain 3 components separted by ".".
    Code
      as_bq_job("x")
    Condition
      Error:
      ! When `"x"` is a string, it must contain 3 components separted by ".".
    Code
      as_bq_dataset("x")
    Condition
      Error:
      ! When `"x"` is a string, it must contain 2 components separted by ".".

# list coercion errors with bad names

    Code
      as_bq_table(list())
    Condition
      Error:
      ! When `list()` is a list, it must have components "projectId", "datasetId", and "tableId".
    Code
      as_bq_dataset(list())
    Condition
      Error:
      ! When `list()` is a list, it must have components "projectId" and "datasetId".
    Code
      as_bq_job(list())
    Condition
      Error:
      ! When `list()` is a list, it must have components "projectId", "jobId", and "location".

