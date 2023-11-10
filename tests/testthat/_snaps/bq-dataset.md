# by default can not delete dataset containing tables

    Code
      bq_dataset_delete(ds)
    Condition
      Error in `bq_delete()`:
      ! Dataset gargle-169921:<DATASET> is still in use [resourceInUse]

