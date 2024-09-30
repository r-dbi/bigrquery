# errors when table is known to be incomplete

    Code
      bq_table_download(tb, n_max = 35000, page_size = 35000, bigint = "integer64",
        api = "json")
    Message
      Downloading first chunk of data.
    Condition
      Error in `bq_table_download()`:
      ! First chunk is incomplete:
      x 35,000 rows were requested, but only {n} rows were received.
      i Leave `page_size` unspecified or use an even smaller value.

# warns if supplying unnused arguments

    Code
      . <- bq_table_download(tb, api = "arrow", page_size = 1, start_index = 1,
        max_connections = 1)
    Condition
      Warning in `bq_table_download()`:
      `page_size` is ignored when `api == "arrow"`
      Warning in `bq_table_download()`:
      `start_index` is ignored when `api == "arrow"`
      Warning in `bq_table_download()`:
      `max_connections` is ignored when `api == "arrow"`

