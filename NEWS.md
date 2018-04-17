# bigrquery 0.4.1.9000

## New API

Complete overhaul of the low-level API to make it easier to use. (The primary motivation is to make the package more enjoyable for me to maintain but it should also be helpful if you need to do anything unsupported by the higher-level DBI and dplyr backends). Changes to the code have been paired with improved documentation and greater unit test coverage.

The old API has been soft-deprecated - it will continue to work, but no further development will occur. It will be formally deprecated in the next version, and then removed in the version after that.

* Consistent naming scheme:
  All API functions now have the form `bq_object_verb()`, e.g. 
  `bq_table_create()`, or `bq_dataset_delete()`. The old API will continue to
  exist, but I highly recommend moving to the new API.

* S3 helper classes:
  `bq_table()`, `bq_dataset()`, `bq_job()`, `bq_field()` and `bq_fields()`
  constructor functions create S3 objects corresponding to important BigQuery 
  objects (#150). These are paired with `as_` coercion functions and used throughout 
  the new API. This deprecates `format_table()`, `format_dataset()`, 
  `parse_table()` and `parse_dataset()`.

* Easier local testing:
  New `bq_test_project()` and `bq_test_dataset()` make it easier to run 
  bigrquery tests locally. All you need to do is create a new BigQuery project,
  set up billing, and make sure the project name is set in the 
  `BIGQUERY_TEST_PROJECT` environment variable, then call `bq_test_init()`.

* More efficient data transer: the new api makes extensive user of the `fields`
  query parameter ensuring the bigrquery only downloads data that it actually 
  uses (#153).

* The new API uses standard SQL. To use `legacy_sql` set `use_legacy_sql = TRUE`
  in the query functions.

### New feautres

* `bq_table_create()` can now specify `fields`. (#204)

* `bq_project_query()` and `bq_dataset_query()` can now supply query parameters
  (#191).
  
* `bq_table_load()` loads data from a Google CloudStorage URI and is paired with 
  `bq_table_save()` to save data to a Google CloudStorage URI (#155)

## Improved downloads

I have substantially improved the performance and flexibility of downloading data from a table/query.

* The two steps (downloading and parsing) now happen in sequence, rather than
  in parallel. For large downloads, this means that you'll now see two progress
  bars: one for download and one for parsing.
  
* Downloads now occur in parallel, using up to 6 simultaneous connections by 
  default.

* The parsing code has been rewritten in C++. As well as considerably improving 
  performance, this also adds supported for nested (record/struct) and repeated 
  (array) columns (#145). These columns will yield list-columns in the 
  following form:
  
    * Repeated values become list-columns containing vectors.
    * Records become list-columns containing named lists
    * Repeated records become list-columns containing data frames

In my benchmark experiment, I can now download the first million rows of `publicdata.samples.natality` in about a minute. This data frame is about 170 MB in BigQuery and 140 MB in R; a minute to download this much data seems fairly reasonable to me. The bottleneck for loading BigQuery data is now parsing BigQuery's json format. I don't see any obvious ways to make the performance faster as I'm already using the fastest C++ json parser, RapidJson. If you need to download gigabytes of data, I recommending use `bq_table_save()` to export a csv file to google cloud storage, and using the `gsutil` command line utility to download it, and then `readr::read_csv()` or `data.table::fread()` to read it. Unfortunately you can not export nested or repeated formats into CSV, but I am not aware of any R packages that will efficiently load the arvo or ndjson formats that BigQuery will produce.

Results are now returned as tibbles because the base print method does not handle list columns well.

## Other bug fixes and improvements

* The dplyr interface can work with literal SQL once more (#218).

* `dplyr::compute()` now works (@realAkhmed, #52).

* The DBI interface has been updated to use the new low-level API.
  This means that it defaults to using modern SQL not legacy SQL.
  Use `use_legacy_sql = TRUE` in `DBI::dbConnect()` if you need to use
  legacy SQL (#147).

* `query_exec()` fixed error caused by progress bar for queries with empty result 
   (@byapparov, #206)

* Request error messages will now contain the "reason", which can contain 
  useful information for debugging (#209).

* Improved SQL translation for `pmax()`, `pmin()`, `sd()`, `all()`, and `any()`
  (#176, #179, @jarodmeng). And for `paste0()`, `cor()` and `cov()`
  (@edgararuiz).

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
