# Version 0.3.0.9000

* `list_tabledata()` and `query_exec()` now give a nicer progress bar, 
  including estimated time remaining (#100).

* All POST requests (inserts, updates, copies and `query_exec`) now 
  take `...`. This allows you to add arbitrary additional data to the 
  request body making it possible to use parts of the BigQuery API 
  that are otherwise not exposed (#149). `snake_case` argument names are
  automatically converted to `camelCase` so you can stick consistently 
  to snake case in your R code.

* `list_tables()` (#108) and `list_datasets()` (#141) are now paginated.
  By default they retrieve 50 items per page, and will iterate until they
  get everything.

* New `insert_table()` allows you to insert empty tables into a dataset.

* `insert_query_job()` and `query_exec()` gain a `use_legacy_sql` option
  so you can opt-in to the legacy SQL system (#124, @backlin)

* Now supports DATE, TIME, and DATETIME types (#128). 

* fixed for dplyr 0.6.0 compatibility

* `set_oauth2.0_cred()` allows user to supply their own Google OAuth application when setting credentials (#130, @jarodmeng)

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
