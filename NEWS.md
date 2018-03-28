# bigrquery 0.4.1.9000

* Add `insert_load_csv_from_cs_job()` and `load_csv_from_cs()` functions to help
  load CSV files stored in Google Cloud Storage into BigQuery table.

# Version 0.4.1

* `query_exec()` and `insert_query_job()` gain `quiet` arguments so you can
  supppress the progress bars if needed.
  
* Profiling revealed that ~40% of the time taken by `query_exec()` was
  a single line inside a function that helps parse BigQuery's json into an R 
  data frame. I replaced that with a C function that is much much faster.

* Fix SQL translation omissions discovered by dbplyr 1.1.0

# Version 0.4.0

## New features

* dplyr support has been updated to require dplyr 0.7.0 and use dbplyr. This
  means that you can now more naturally work directly with DBI connections.
  dplyr now also uses modern BigQuery SQL which supports a broader set of
  translations. Along the way I've also fixed some SQL generation bugs (#48).

* The DBI driver gets a new name: `bigquery()`.

* New `insert_extract_job()` make it possible to extract data and save in 
  google storage (@realAkhmed, #119).

* New `insert_table()` allows you to insert empty tables into a dataset.

* All POST requests (inserts, updates, copies and `query_exec`) now 
  take `...`. This allows you to add arbitrary additional data to the 
  request body making it possible to use parts of the BigQuery API 
  that are otherwise not exposed (#149). `snake_case` argument names are
  automatically converted to `camelCase` so you can stick consistently 
  to snake case in your R code.

* Full support for DATE, TIME, and DATETIME types (#128). 

## Big fixes and minor improvements

* All bigrquery requests now have a custom user agent that specifies the
  versions of bigrquery and httr that are used (#151).

* `dbConnect()` gains new `use_legacy_sql`, `page_size`, and `quiet` arguments 
  that are passed onto `query_exec()`. These allow you to control query options 
  at the connection level.

* `insert_upload_job()` now sends data in newline-delimited JSON instead
  of csv (#97). This should be considerably faster and avoids character
  encoding issues (#45). `POSIXlt` columns are now also correctly 
  coerced to TIMESTAMPS (#98).

* `insert_query_job()` and `query_exec()` gain new arguments:

    * `quiet = TRUE` will suppress the progress bars if needed.
    * `use_legacy_sql = FALSE` option allows you to opt-out of the 
      legacy SQL system (#124, @backlin)

* `list_tables()` (#108) and `list_datasets()` (#141) are now paginated.
  By default they retrieve 50 items per page, and will iterate until they
  get everything.

* `list_tabledata()` and `query_exec()` now give a nicer progress bar, 
  including estimated time remaining (#100).

* `query_exec()` should be considerably faster because profiling revealed that 
  ~40% of the time taken by was a single line inside a function that helps 
  parse BigQuery's json into an R data frame. I replaced the slow R code with
  a faster C function.

* `set_oauth2.0_cred()` allows user to supply their own Google OAuth 
  application when setting credentials (#130, @jarodmeng)

* `wait_for()` uses now reports the query total bytes billed, which is
  more accurate because it takes into account caching and other factors.

# Version 0.3.0

* New `set_service_token()` allows you to use OAuth service token instead of
  interactive authentication.from

* `^` is correctly translated to `pow()` (#110).

* Provide full DBI compliant interface (@krlmlr).

* Backend now translates `iflese()` to `IF` (@realAkhmed, #53).
  
# Version 0.2.0.

* Compatiable with latest httr.

* Computation of the SQL data type that corresponds to a given R object 
  is now more robust against unknown classes. (#95, @krlmlr)

* A data frame with full schema information is returned for zero-row results.
  (#88, @krlmlr)

* New `exists_table()`. (#91, @krlmlr)

* New arguments `create_disposition` and `write_disposition` to
  `insert_upload_job()`. (#92, @krlmlr)

* Renamed option `bigquery.quiet` to `bigrquery.quiet`. (#89, @krlmlr)

* New `format_dataset()` and `format_table()`. (#81, @krlmlr)

* New `list_tabledata_iter()` that allows fetching a table in chunks of 
  varying size. (#77, #87, @krlmlr)

* Add support for API keys via the `BIGRQUERY_API_KEY` environment variable. 
  (#49)
