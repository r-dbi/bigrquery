# errors when table is known to be incomplete

    Code
      bq_table_download(tb, n_max = 35000, page_size = 35000, bigint = "integer64")
    Message
      Downloading first chunk of data.
    Condition
      Error in `bq_table_download()`:
      ! First chunk is incomplete:
      x 35,000 rows were requested, but only {n} rows were received.
      i Leave `page_size` unspecified or use an even smaller value.

