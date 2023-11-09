# bq_project_query inputs are checked

    Code
      bq_project_query(1)
    Condition
      Error in `bq_project_query()`:
      ! `x` must be a single string, not the number 1.
    Code
      bq_project_query("abc", 1)
    Condition
      Error in `bq_project_query()`:
      ! `query` must be a single string, not the number 1.
    Code
      bq_project_query("abc", "SELECT *", destination_table = 1)
    Condition
      Error in `bq_project_query()`:
      ! `destination_table` must be a string, list, or `bq_table()`.
    Code
      bq_project_query("abc", "SELECT *", destination_table = "a")
    Condition
      Error in `bq_project_query()`:
      ! When `destination_table` is a string, it must contain 3 components separted by ".".
    Code
      bq_project_query("abc", "SELECT *", destination_table = list())
    Condition
      Error in `bq_project_query()`:
      ! When `destination_table` is a list, it must have components "projectId", "datasetId", and "tableId".
    Code
      bq_project_query("abc", "SELECT *", quiet = 1)
    Condition
      Error in `bq_project_query()`:
      ! `quiet` must be `TRUE`, `FALSE`, or `NA`, not the number 1.

# bq_dataset_query inputs are checked

    Code
      bq_dataset_query(1)
    Condition
      Error in `bq_dataset_query()`:
      ! `x` must be a string, list, or `bq_dataset()`.
    Code
      bq_dataset_query("abc")
    Condition
      Error in `bq_dataset_query()`:
      ! When `x` is a string, it must contain 2 components separted by ".".
    Code
      bq_dataset_query("abc.def", 1)
    Condition
      Error in `bq_dataset_query()`:
      ! `query` must be a single string, not the number 1.
    Code
      bq_dataset_query("abc.def", "SELECT *", destination_table = 1)
    Condition
      Error in `bq_dataset_query()`:
      ! `destination_table` must be a string, list, or `bq_table()`.
    Code
      bq_dataset_query("abc.def", "SELECT *", destination_table = "a")
    Condition
      Error in `bq_dataset_query()`:
      ! When `destination_table` is a string, it must contain 3 components separted by ".".
    Code
      bq_dataset_query("abc.def", "SELECT *", destination_table = list())
    Condition
      Error in `bq_dataset_query()`:
      ! When `destination_table` is a list, it must have components "projectId", "datasetId", and "tableId".
    Code
      bq_dataset_query("abc.def", "SELECT *", billing = 1)
    Condition
      Error in `bq_dataset_query()`:
      ! `billing` must be a single string or `NULL`, not the number 1.
    Code
      bq_dataset_query("abc.def", "SELECT *", quiet = 1)
    Condition
      Error in `bq_dataset_query()`:
      ! `quiet` must be `TRUE`, `FALSE`, or `NA`, not the number 1.

