# errors when table is known to be incomplete

    Code
      bq_table_download(tb, n_max = 35000, page_size = 35000, bigint = "integer64")
    Condition
      Error in `bq_table_download()`:
      ! First chunk is incomplete:
      x 35,000 rows were requested, but only 31,977 rows were received.
      i Leave `page_size` unspecified or use an even smaller value.

