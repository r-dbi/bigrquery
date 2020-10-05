# bigrquery 1.3.2

* BigQuery `BYTES` and `GEOGRAPHY` column types are now supported via
  the [blob](https://blob.tidyverse.org/) and 
  [wk](https://paleolimbot.github.io/wk/) packages, respectively
  (@paleolimbot, #354, #388).

* When used with dbplyr >= 2.0.0, ambiguous variables in joins will get
  suffixes `_x` and `_y` (instead of `.x` and `.y` which don't work with
  BigQuery) (#403).

* `bq_table_download()` works once again with large row counts
  (@gjuggler, #395). Google's API has stopped accepting `startIndex`
  parameters with scientific formatting, which was happening for large
  values (>1e5) by default.

* New `bq_perform_query_dry_run()` to retrieve the estimated cost of
  performing a query (@Ka2wei, #316).

# bigrquery 1.3.1

* Now requires gargle 0.5.0

# bigrquery 1.3.0

* Old functions (not starting with `bq_`) are deprecated (@byapparov, #335)

* When `bq_perform_*()` fails, you now see all errors, not just the first (#355).

* `bq_perform_query()` can now execute parameterised query with parameters 
  of `ARRAY` type (@byapparov, #303). Vectors of length > 1 will be
  automatically converted to `ARRAY` type, or use `bq_param_array()` to
  be explicit.

* `bq_perform_upload()` works once again (#361). It seems like the generated
  JSON was always incorrect, but Google's type checking only recently become
  strict enough to detect the problem.

* `dbExecute()` is better supported. It no longer fails with a spurious
  error for DDL queries, and it returns the number of affected rows for
  DML queries (#375).

* `dbSendQuery()` (and hence `dbGetQuery()`) and `collect()` passes on `...` 
  to `bq_perform_query()`. `collect()` gains `page_size` and `max_connection` 
  arguments that are passed on to `bq_table_download()` (#374).

* `copy_to()` now works with BigQuery (although it doesn't support temporary
  tables so application is somewhat limited) (#337).
  
* `str_detect()` now correctly translated to `REGEXP_CONTAINS`  
  (@jimmyg3g, #369).

* Error messages inlude hints for common problems (@deflaux, #353).

# bigrquery 1.2.0

## Auth from gargle

bigrquery's auth functionality now comes from the [gargle package](https://gargle.r-lib.org), which provides R infrastructure to work with Google APIs, in general. The same transition is underway in several other packages, such as [googledrive](https://googledrive.tidyverse.org). This will make user interfaces more consistent and makes two new token flows available in bigrquery:

  * Application Default Credentials
  * Service account tokens from the metadata server available to VMs running on GCE
  
Where to learn more:
  
  * Help for [`bq_auth()`](https://bigrquery.r-dbi.org/reference/bq_auth.html) *all that most users need*
  * *details for more advanced users*
    - [How gargle gets tokens](https://gargle.r-lib.org/articles/how-gargle-gets-tokens.html)
    - [Non-interactive auth](https://gargle.r-lib.org/articles/non-interactive-auth.html)
    - [How to get your own API credentials](https://gargle.r-lib.org/articles/get-api-credentials.html) 

### Changes that a user will notice

Temporary files are now deleted after table download. (@meztez, #343)

OAuth2 tokens are now cached at the user level, by default, instead of in `.httr-oauth` in the current project. The default OAuth app has also changed. This means you will need to re-authorize bigrquery (i.e. get a new token). You may want to delete any vestigial `.httr-oauth` files lying around your bigrquery projects.

The OAuth2 token key-value store now incorporates the associated Google user when indexing, which makes it easier to switch between Google identities.

`bq_user()` is a new function that reveals the email of the user associated with the current token.

If you previously used `set_service_token()` to use a service account token, it still works. But you'll get a deprecation warning. Switch over to `bq_auth(path = "/path/to/your/service-account.json")`. Several other functions are similarly soft-deprecated.

## Dependency changes

R 3.1 is no longer explicitly supported or tested. Our general practice is to support the current release (3.6), devel, and the 4 previous versions of R (3.5, 3.4, 3.3, 3.2).

gargle and rlang are newly Imported.

# bigrquery 1.1.1

* Fix test failure with dbplyr 1.4.0.

* `bq_field()` can now pass `description` parameter which will be applied
  in `bq_table_create()` call (@byapparov, #272).
  
* `bq_table_patch()` - allows to patch table (@byapparov, #253) with new schema.


# bigrquery 1.1.0

## Improved type support

* `bq_table_download()` and the `DBI::dbConnect` method now has a `bigint` 
  argument which governs how BigQuery integer columns are imported into R. As 
  before, the default is `bigint = "integer"`. You can set 
  `bigint = "integer64"` to import BigQuery integer columns as 
  `bit64::integer64` columns in R which allows for values outside the range of 
  `integer` (`-2147483647` to `2147483647`) (@rasmusab, #94).

* `bq_table_download()` now treats NUMERIC columns the same was as FLOAT 
  columns (@paulsendavidjay, #282).

* `bq_table_upload()` works with POSIXct/POSIXct varibles (#251)

## SQL translation

* `as.character()` now translated to `SAFE_CAST(x AS STRING)` (#268).

* `median()` now translates to `APPROX_QUANTILES(x, 2)[SAFE_ORDINAL(2)]` (@valentinumbach, #267).

## Minor bug fixes and improvements

* Jobs now print their ids while running (#252)

* `bq_job()` tracks location so bigrquery now works painlessly with non-US/EU
  locations (#274).

* `bq_perform_upload()` will only autodetect a schema if the table does 
  not already exist.

* `bq_table_download()` correctly computes page ranges if both `max_results`
  and `start_index` are supplied (#248)

* Unparseable date times return NA (#285)

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
