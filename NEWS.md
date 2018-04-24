# bigrquery 1.0.0

## Improved downloads

The system for downloading data from BigQuery into R has been rewritten from the ground up to give considerable improvements in performance and flexibility.

* The two steps, downloading and parsing, now happen in sequence, rather than
  interleaved. This means that you'll now see two progress bars: one for 
  downloading JSON from BigQuery and one for parsing that JSON into a data 
  frame.
  
* Downloads now occur in parallel, using up to 6 simultaneous connections by 
  default.

* The parsing code has been rewritten in C++. As well as considerably improving 
  performance, this also adds support for nested (record/struct) and repeated 
  (array) columns (#145). These columns will yield list-columns in the 
  following forms:
  
    * Repeated values become list-columns containing vectors.
    * Nested values become list-columns containing named lists.
    * Repeated nested values become list-columns containing data frames.

* Results are now returned as tibbles, not data frames, because the base print 
  method does not handle list columns well.

I can now download the first million rows of `publicdata.samples.natality` in about a minute. This data frame is about 170 MB in BigQuery and 140 MB in R; a minute to download this much data seems reasonable to me. The bottleneck for loading BigQuery data is now parsing BigQuery's json format. I don't see any obvious way to make this faster as I'm already using the fastest C++ json parser, [RapidJson](http://rapidjson.org). If this is still too slow for you (i.e. you're downloading GBs of data), see `?bq_table_download` for an alternative approach.

## New features

### dplyr

* `dplyr::compute()` now works (@realAkhmed, #52).

* `tbl()` now accepts fully (or partially) qualified table names, like
  "publicdata.samples.shakespeare" or "samples.shakespeare". This makes it 
  possible to join tables across datasets (#219).

### DBI

* `dbConnect()` now defaults to standard SQL, rather than legacy SQL. Use 
  `use_legacy_sql = TRUE` if you need the previous behaviour (#147).

* `dbConnect()` now allows `dataset` to be omitted; this is natural when you 
  want to use tables from multiple datasets.
  
* `dbWriteTable()` and `dbReadTable()` now accept fully (or partially) 
  qualified table names.

* `dbi_driver()` is deprecated; please use `bigquery()` instead.

### Low-level API

The low-level API has been completely overhauled to make it easier to use. The primary motivation was to make bigrquery development more enjoyable for me, but it should also be helpful to you when you need to go outside of the features provided by higher-level DBI and dplyr interfaces. The old API has been soft-deprecated - it will continue to work, but no further development will occur (including bug fixes). It will be formally deprecated in the next version, and then removed in the version after that.

* __Consistent naming scheme__:
  All API functions now have the form `bq_object_verb()`, e.g. 
  `bq_table_create()`, or `bq_dataset_delete()`.

* __S3 classes__:
  `bq_table()`, `bq_dataset()`, `bq_job()`, `bq_field()` and `bq_fields()`
  constructor functions create S3 objects corresponding to important BigQuery 
  objects (#150). These are paired with `as_` coercion functions and used throughout 
  the new API.

* __Easier local testing__:
  New `bq_test_project()` and `bq_test_dataset()` make it easier to run 
  bigrquery tests locally. To run the tests yourself, you need to create a 
  BigQuery project, and then follow the instructions in `?bq_test_project`.

* __More efficient data transfer__: 
  The new API makes extensive use of the `fields` query parameter, ensuring 
  that functions only download data that they actually use (#153).

* __Tighter GCS connection__: 
  New `bq_table_load()` loads data from a Google Cloud Storage URI, pairing 
  with `bq_table_save()` which saves data to a GCS URI (#155).

## Bug fixes and minor improvements

### dplyr

* The dplyr interface can work with literal SQL once more (#218).

* Improved SQL translation for `pmax()`, `pmin()`, `sd()`, `all()`, and `any()`
  (#176, #179, @jarodmeng). And for `paste0()`, `cor()` and `cov()`
  (@edgararuiz).

* If you have the development version of dbplyr installed, `print()`ing
  a BigQuery table will not perform an unneeded query, but will instead 
  download directly from the table (#226).

### Low-level

* Request error messages now contain the "reason", which can contain 
  useful information for debugging (#209).

* `bq_dataset_query()` and `bq_project_query()` can now supply query parameters
  (#191).

* `bq_table_create()` can now specify `fields` (#204).

* `bq_perform_query()` no longer fails with empty results (@byapparov, #206).

# Version 0.4.1

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

* `list_tabledata` returns empty table on max_pages=0 (#184, @ras44 @byapparov)

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
