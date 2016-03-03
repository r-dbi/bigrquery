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
