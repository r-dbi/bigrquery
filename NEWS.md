# bigrquery (development version)

# bigrquery 1.5.0

## Major changes

* bigrquery is now MIT licensed (#453).

* Deprecated functions (i.e. those not starting with `bq_`) have been
  removed (#551). These have been superseded for a long time and were formally 
  deprecated in bigrquery 1.3.0 (2020).

* `bq_table_download()` now returns unknown fields as character vectors.
  This means that BIGNUMERIC (#435) and JSON (#544) data is downloaded into
  R for you to process as you wish.
  
  It now parses dates using the clock package. This leads to a considerable
  performance improvement (#430) and ensures that dates prior to 1970-01-01 are
  parsed correctly (#285).

## Significant DBI improvements

* bigquery datasets and tables will now appear in the connection pane when 
  using `dbConnect` (@meztez, #431).

* `dbAppendTable()` (#539), `dbCreateTable()` (#483), and `dbExecute` (#502)
  are now supported.

* `dbGetQuery()`/`dbSendQuery()` gains support for parameterised queries via 
  the `params` argument (@byapparov, #444).

* `dbReadTable()`, `dbWriteTable()`, `dbExistsTable()`, `dbRemoveTable()`,
  and `dbListFields()` now all work with `DBI::Id()` (#537).

## Significant dbplyr improvements

* bigrquery now uses 2nd edition of dbplyr interface (#508) and is
  compatible with dbplyr 2.4.0 (#550).

* Joins now work correctly across bigrquery connections (#433).

* `grepl(pattern, x)` is now correctly translated to 
  `REGEXP_CONTAINS(x, pattern)` (#416).

* `median()` gets a translation that works in `summarise()` and a clear
  error if you use it in `mutate()` (#419).

* `tbl()` now works with views (#519), including the views found in the 
  `INFORMATION_SCHEMA` schema (#468).

* `tbl(con, sql("..."))` now works robustly once more (#540), fixing the
  "URL using bad/illegal format or missing URL" error.

* `runif(n())` gains a translation so that `slice_sample()` can work
  (@mgirlich, #448).

## Minor improvements and bug fixes

* Google API URLs have been aligned with the Google Cloud Discovery docs. This
  enables support for Private and Restricted Google APIs configurations 
  (@husseyd, #541)

* Functions generally try to do a better job of telling you when you've 
  supplied the wrong type of input. Additionally, if you supply `SQL()` to
  a query, you no longer get a weird warning (#498).

* If `bq_job_wait()` receives a 503 response, it now waits for 2 seconds and
  tries again (#535).

* `dbFetch()` now respects the `quiet` setting from the connection (#463).

* `dbGetRowCount()` and `dbHasComplete()` now return correct values when you
  try to fetch more rows than actually exist (#501).

* New `dbQuoteLiteral()` method for logicals reverts breaking change introduced 
  by DBI 1.1.2 (@meztez, #478).

* `dbWriteTable()` now correct uses the `billing` value set in the 
  connection (#486).

# bigrquery 1.4.2

* Sync up with the current release of gargle (1.4.0). Recently gargle
  introduced some changes around OAuth and bigrquery is syncing with up that:

  - `bq_oauth_client()` is a new function to replace the now-deprecated
    `bq_oauth_app()`.
  -  The new `client` argument of `bq_auth_configure()` replaces the
     now-deprecated `client` argument.
  -  The documentation of `bq_auth_configure()` emphasizes that the preferred
     way to "bring your own OAuth client" is by providing the JSON downloaded
     from Google Developers Console.

* `op_table.lazy_select_query()` now returns a string instead of a list, which
  fixes an error seen when printing or using functions like `head()` or
  `dplyr::glimpse()` (@clente, #509).

# bigrquery 1.4.1

* Fix for `R CMD check` in R-devel (#511)

* bigrquery is now compatible with dbplyr 2.2.0 (@mgirlich, #495).

* brio is new in Imports, replacing the use of the Suggested package readr, 
  in `bq_table_download()` (@AdeelK93, #462).

# bigrquery 1.4.0

* `bq_table_download()` has been heavily refactored (#412):

  - It should now return the requested results, in full, in most situations.
    However, when there is a "row shortage", it throws an error instead of
    silently returning incomplete results.
  - The `max_results` argument has been deprecated in favor of `n_max`, which
    reflects what we actually do with this number and is consistent with the
    `n_max` argument elsewhere, e.g., `readr::read_csv()`.
  - The default value of `page_size` is no longer fixed and, instead, is
    determined empirically. Users are strongly recommended to let bigrquery
    select `page_size` automatically, unless there's a specific reason to do
    otherwise.

* The `BigQueryResult` object gains a `billing` slot (@meztez, #423).

* `collect.tbl_BigQueryConnection()` honours the `bigint` field found in a connection object created with `DBI::dbConnect()` and passes `bigint` along to `bq_table_download()`. This improves support for 64-bit integers when reading BigQuery tables with dplyr syntax (@zoews, #439, #437).

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

* Error messages include hints for common problems (@deflaux, #353).

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

* `bq_table_upload()` works with POSIXct/POSIXct variables (#251)

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

* Compatible with latest httr.

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
