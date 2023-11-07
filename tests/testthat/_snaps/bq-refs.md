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

# string coercion error on invalid number of components

    Code
      as_bq_table("x")
    Condition
      Error:
      ! Character <bq_table> must contain 3 components when split by `.`
    Code
      as_bq_table("a.b.c.d")
    Condition
      Error:
      ! Character <bq_table> must contain 3 components when split by `.`
    Code
      as_bq_job("x")
    Condition
      Error:
      ! Character <bq_job> must contain 3 components when split by `.`
    Code
      as_bq_dataset("x")
    Condition
      Error:
      ! Character <bq_dataset> must contain 2 components when split by `.`

# list coercion errors with bad names

    Code
      as_bq_table(list())
    Condition
      Error:
      ! List <bq_table> must have components projectId, datasetId and tableId
    Code
      as_bq_dataset(list())
    Condition
      Error:
      ! List <bq_dataset> must have components projectId and datasetId
    Code
      as_bq_job(list())
    Condition
      Error:
      ! List <bq_job> must have components projectId, jobId and location

