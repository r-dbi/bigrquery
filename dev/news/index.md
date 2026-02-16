# Changelog

## bigrquery (development version)

- [`dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html) gains
  `params=` support ([@r2evans](https://github.com/r2evans),
  [\#667](https://github.com/r-dbi/bigrquery/issues/667)).
- [`dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html) and
  [`dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
  error if you accidentally use `parameters`
  ([@r2evans](https://github.com/r2evans),
  [\#667](https://github.com/r-dbi/bigrquery/issues/667)).
- Check `getOption("bigrquery.quiet")` option in more `bq_*` functions
  ([@r2evans](https://github.com/r2evans),
  [\#663](https://github.com/r-dbi/bigrquery/issues/663)).

## bigrquery 1.6.1

CRAN release: 2025-09-10

- Fix test failure on CRAN.

## bigrquery 1.6.0

CRAN release: 2025-09-03

### New features

- If the bigrquerystorage package is installed,
  [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  (and hence
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html),
  [`dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html) and
  [`dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html)) will use
  it. This will drastically improve the speed of downloading large
  datasets. A big thanks to [@meztez](https://github.com/meztez) for
  creating the bigrquerystorage package!

### Bug fixes and minor improvements

- Various R CMD check fixes

- Functions and arguments deprecated in bigrquery 1.4.0 (released) have
  now been removed.

- The
  [`bq_perform_upload()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)
  function now allows users to choose the transmission format (JSON or
  PARQUET) for data sent to BigQuery
  ([@apalacio9502](https://github.com/apalacio9502),
  [\#608](https://github.com/r-dbi/bigrquery/issues/608)).

- bigrquery now requires R 4.1, inline with our version support
  principles.

## bigrquery 1.5.1

CRAN release: 2024-03-14

- Forward compatibility with upcoming dbplyr release
  ([\#601](https://github.com/r-dbi/bigrquery/issues/601)).

## bigrquery 1.5.0

CRAN release: 2024-01-22

### Major changes

- bigrquery is now MIT licensed
  ([\#453](https://github.com/r-dbi/bigrquery/issues/453)).

- Deprecated functions (i.e. those not starting with `bq_`) have been
  removed ([\#551](https://github.com/r-dbi/bigrquery/issues/551)).
  These have been superseded for a long time and were formally
  deprecated in bigrquery 1.3.0 (2020).

- [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  now returns unknown fields as character vectors. This means that
  BIGNUMERIC ([\#435](https://github.com/r-dbi/bigrquery/issues/435))
  and JSON ([\#544](https://github.com/r-dbi/bigrquery/issues/544)) data
  is downloaded into R for you to process as you wish.

  It now parses dates using the clock package. This leads to a
  considerable performance improvement
  ([\#430](https://github.com/r-dbi/bigrquery/issues/430)) and ensures
  that dates prior to 1970-01-01 are parsed correctly
  ([\#285](https://github.com/r-dbi/bigrquery/issues/285)).

### Significant DBI improvements

- bigquery datasets and tables will now appear in the connection pane
  when using `dbConnect` ([@meztez](https://github.com/meztez),
  [\#431](https://github.com/r-dbi/bigrquery/issues/431)).

- [`dbAppendTable()`](https://dbi.r-dbi.org/reference/dbAppendTable.html)
  ([\#539](https://github.com/r-dbi/bigrquery/issues/539)),
  [`dbCreateTable()`](https://dbi.r-dbi.org/reference/dbCreateTable.html)
  ([\#483](https://github.com/r-dbi/bigrquery/issues/483)), and
  `dbExecute` ([\#502](https://github.com/r-dbi/bigrquery/issues/502))
  are now supported.

- [`dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html)/[`dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
  gains support for parameterised queries via the `params` argument
  ([@byapparov](https://github.com/byapparov),
  [\#444](https://github.com/r-dbi/bigrquery/issues/444)).

- [`dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html),
  [`dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html),
  [`dbExistsTable()`](https://dbi.r-dbi.org/reference/dbExistsTable.html),
  [`dbRemoveTable()`](https://dbi.r-dbi.org/reference/dbRemoveTable.html),
  and
  [`dbListFields()`](https://dbi.r-dbi.org/reference/dbListFields.html)
  now all work with
  [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html)
  ([\#537](https://github.com/r-dbi/bigrquery/issues/537)).

### Significant dbplyr improvements

- bigrquery now uses 2nd edition of dbplyr interface
  ([\#508](https://github.com/r-dbi/bigrquery/issues/508)) and is
  compatible with dbplyr 2.4.0
  ([\#550](https://github.com/r-dbi/bigrquery/issues/550)).

- Joins now work correctly across bigrquery connections
  ([\#433](https://github.com/r-dbi/bigrquery/issues/433)).

- `grepl(pattern, x)` is now correctly translated to
  `REGEXP_CONTAINS(x, pattern)`
  ([\#416](https://github.com/r-dbi/bigrquery/issues/416)).

- [`median()`](https://rdrr.io/r/stats/median.html) gets a translation
  that works in
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  and a clear error if you use it in
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  ([\#419](https://github.com/r-dbi/bigrquery/issues/419)).

- [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) now works
  with views ([\#519](https://github.com/r-dbi/bigrquery/issues/519)),
  including the views found in the `INFORMATION_SCHEMA` schema
  ([\#468](https://github.com/r-dbi/bigrquery/issues/468)).

- `tbl(con, sql("..."))` now works robustly once more
  ([\#540](https://github.com/r-dbi/bigrquery/issues/540)), fixing the
  “URL using bad/illegal format or missing URL” error.

- `runif(n())` gains a translation so that
  [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
  can work ([@mgirlich](https://github.com/mgirlich),
  [\#448](https://github.com/r-dbi/bigrquery/issues/448)).

### Minor improvements and bug fixes

- Google API URLs have been aligned with the Google Cloud Discovery
  docs. This enables support for Private and Restricted Google APIs
  configurations ([@husseyd](https://github.com/husseyd),
  [\#541](https://github.com/r-dbi/bigrquery/issues/541))

- Functions generally try to do a better job of telling you when you’ve
  supplied the wrong type of input. Additionally, if you supply
  [`SQL()`](https://dbi.r-dbi.org/reference/SQL.html) to a query, you no
  longer get a weird warning
  ([\#498](https://github.com/r-dbi/bigrquery/issues/498)).

- If
  [`bq_job_wait()`](https://bigrquery.r-dbi.org/dev/reference/api-job.md)
  receives a 503 response, it now waits for 2 seconds and tries again
  ([\#535](https://github.com/r-dbi/bigrquery/issues/535)).

- [`dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html) now
  respects the `quiet` setting from the connection
  ([\#463](https://github.com/r-dbi/bigrquery/issues/463)).

- [`dbGetRowCount()`](https://dbi.r-dbi.org/reference/dbGetRowCount.html)
  and `dbHasComplete()` now return correct values when you try to fetch
  more rows than actually exist
  ([\#501](https://github.com/r-dbi/bigrquery/issues/501)).

- New
  [`dbQuoteLiteral()`](https://dbi.r-dbi.org/reference/dbQuoteLiteral.html)
  method for logicals reverts breaking change introduced by DBI 1.1.2
  ([@meztez](https://github.com/meztez),
  [\#478](https://github.com/r-dbi/bigrquery/issues/478)).

- [`dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
  now correct uses the `billing` value set in the connection
  ([\#486](https://github.com/r-dbi/bigrquery/issues/486)).

## bigrquery 1.4.2

CRAN release: 2023-04-20

- Sync up with the current release of gargle (1.4.0). Recently gargle
  introduced some changes around OAuth and bigrquery is syncing with up
  that:

  - [`bq_oauth_client()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md)
    is a new function to replace the now-deprecated `bq_oauth_app()`.
  - The new `client` argument of
    [`bq_auth_configure()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md)
    replaces the now-deprecated `client` argument.
  - The documentation of
    [`bq_auth_configure()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md)
    emphasizes that the preferred way to “bring your own OAuth client”
    is by providing the JSON downloaded from Google Developers Console.

- `op_table.lazy_select_query()` now returns a string instead of a list,
  which fixes an error seen when printing or using functions like
  [`head()`](https://rdrr.io/r/utils/head.html) or
  [`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html)
  ([@clente](https://github.com/clente),
  [\#509](https://github.com/r-dbi/bigrquery/issues/509)).

## bigrquery 1.4.1

CRAN release: 2022-10-27

- Fix for `R CMD check` in R-devel
  ([\#511](https://github.com/r-dbi/bigrquery/issues/511))

- bigrquery is now compatible with dbplyr 2.2.0
  ([@mgirlich](https://github.com/mgirlich),
  [\#495](https://github.com/r-dbi/bigrquery/issues/495)).

- brio is new in Imports, replacing the use of the Suggested package
  readr, in
  [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  ([@AdeelK93](https://github.com/AdeelK93),
  [\#462](https://github.com/r-dbi/bigrquery/issues/462)).

## bigrquery 1.4.0

CRAN release: 2021-08-05

- [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  has been heavily refactored
  ([\#412](https://github.com/r-dbi/bigrquery/issues/412)):

  - It should now return the requested results, in full, in most
    situations. However, when there is a “row shortage”, it throws an
    error instead of silently returning incomplete results.
  - The `max_results` argument has been deprecated in favor of `n_max`,
    which reflects what we actually do with this number and is
    consistent with the `n_max` argument elsewhere, e.g.,
    [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).
  - The default value of `page_size` is no longer fixed and, instead, is
    determined empirically. Users are strongly recommended to let
    bigrquery select `page_size` automatically, unless there’s a
    specific reason to do otherwise.

- The `BigQueryResult` object gains a `billing` slot
  ([@meztez](https://github.com/meztez),
  [\#423](https://github.com/r-dbi/bigrquery/issues/423)).

- [`collect.tbl_BigQueryConnection()`](https://bigrquery.r-dbi.org/dev/reference/collect.tbl_BigQueryConnection.md)
  honours the `bigint` field found in a connection object created with
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
  and passes `bigint` along to
  [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md).
  This improves support for 64-bit integers when reading BigQuery tables
  with dplyr syntax ([@zoews](https://github.com/zoews),
  [\#439](https://github.com/r-dbi/bigrquery/issues/439),
  [\#437](https://github.com/r-dbi/bigrquery/issues/437)).

## bigrquery 1.3.2

CRAN release: 2020-10-05

- BigQuery `BYTES` and `GEOGRAPHY` column types are now supported via
  the [blob](https://blob.tidyverse.org/) and
  [wk](https://paleolimbot.github.io/wk/) packages, respectively
  ([@paleolimbot](https://github.com/paleolimbot),
  [\#354](https://github.com/r-dbi/bigrquery/issues/354),
  [\#388](https://github.com/r-dbi/bigrquery/issues/388)).

- When used with dbplyr \>= 2.0.0, ambiguous variables in joins will get
  suffixes `_x` and `_y` (instead of `.x` and `.y` which don’t work with
  BigQuery) ([\#403](https://github.com/r-dbi/bigrquery/issues/403)).

- [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  works once again with large row counts
  ([@gjuggler](https://github.com/gjuggler),
  [\#395](https://github.com/r-dbi/bigrquery/issues/395)). Google’s API
  has stopped accepting `startIndex` parameters with scientific
  formatting, which was happening for large values (\>1e5) by default.

- New
  [`bq_perform_query_dry_run()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)
  to retrieve the estimated cost of performing a query
  ([@Ka2wei](https://github.com/Ka2wei),
  [\#316](https://github.com/r-dbi/bigrquery/issues/316)).

## bigrquery 1.3.1

CRAN release: 2020-05-15

- Now requires gargle 0.5.0

## bigrquery 1.3.0

CRAN release: 2020-05-08

- Old functions (not starting with `bq_`) are deprecated
  ([@byapparov](https://github.com/byapparov),
  [\#335](https://github.com/r-dbi/bigrquery/issues/335))

- When `bq_perform_*()` fails, you now see all errors, not just the
  first ([\#355](https://github.com/r-dbi/bigrquery/issues/355)).

- [`bq_perform_query()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)
  can now execute parameterised query with parameters of `ARRAY` type
  ([@byapparov](https://github.com/byapparov),
  [\#303](https://github.com/r-dbi/bigrquery/issues/303)). Vectors of
  length \> 1 will be automatically converted to `ARRAY` type, or use
  [`bq_param_array()`](https://bigrquery.r-dbi.org/dev/reference/bq_param.md)
  to be explicit.

- [`bq_perform_upload()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)
  works once again
  ([\#361](https://github.com/r-dbi/bigrquery/issues/361)). It seems
  like the generated JSON was always incorrect, but Google’s type
  checking only recently become strict enough to detect the problem.

- [`dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html) is
  better supported. It no longer fails with a spurious error for DDL
  queries, and it returns the number of affected rows for DML queries
  ([\#375](https://github.com/r-dbi/bigrquery/issues/375)).

- [`dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
  (and hence
  [`dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html)) and
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
  passes on `...` to
  [`bq_perform_query()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md).
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html)
  gains `page_size` and `max_connection` arguments that are passed on to
  [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  ([\#374](https://github.com/r-dbi/bigrquery/issues/374)).

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) now
  works with BigQuery (although it doesn’t support temporary tables so
  application is somewhat limited)
  ([\#337](https://github.com/r-dbi/bigrquery/issues/337)).

- `str_detect()` now correctly translated to `REGEXP_CONTAINS`
  ([@jimmyg3g](https://github.com/jimmyg3g),
  [\#369](https://github.com/r-dbi/bigrquery/issues/369)).

- Error messages include hints for common problems
  ([@deflaux](https://github.com/deflaux),
  [\#353](https://github.com/r-dbi/bigrquery/issues/353)).

## bigrquery 1.2.0

CRAN release: 2019-07-02

### Auth from gargle

bigrquery’s auth functionality now comes from the [gargle
package](https://gargle.r-lib.org), which provides R infrastructure to
work with Google APIs, in general. The same transition is underway in
several other packages, such as
[googledrive](https://googledrive.tidyverse.org). This will make user
interfaces more consistent and makes two new token flows available in
bigrquery:

- Application Default Credentials
- Service account tokens from the metadata server available to VMs
  running on GCE

Where to learn more:

- Help for
  [`bq_auth()`](https://bigrquery.r-dbi.org/reference/bq_auth.html) *all
  that most users need*
- *details for more advanced users*
  - [How gargle gets
    tokens](https://gargle.r-lib.org/articles/how-gargle-gets-tokens.html)
  - [Non-interactive
    auth](https://gargle.r-lib.org/articles/non-interactive-auth.html)
  - [How to get your own API
    credentials](https://gargle.r-lib.org/articles/get-api-credentials.html)

#### Changes that a user will notice

Temporary files are now deleted after table download.
([@meztez](https://github.com/meztez),
[\#343](https://github.com/r-dbi/bigrquery/issues/343))

OAuth2 tokens are now cached at the user level, by default, instead of
in `.httr-oauth` in the current project. The default OAuth app has also
changed. This means you will need to re-authorize bigrquery (i.e. get a
new token). You may want to delete any vestigial `.httr-oauth` files
lying around your bigrquery projects.

The OAuth2 token key-value store now incorporates the associated Google
user when indexing, which makes it easier to switch between Google
identities.

[`bq_user()`](https://bigrquery.r-dbi.org/dev/reference/bq_user.md) is a
new function that reveals the email of the user associated with the
current token.

If you previously used `set_service_token()` to use a service account
token, it still works. But you’ll get a deprecation warning. Switch over
to `bq_auth(path = "/path/to/your/service-account.json")`. Several other
functions are similarly soft-deprecated.

### Dependency changes

R 3.1 is no longer explicitly supported or tested. Our general practice
is to support the current release (3.6), devel, and the 4 previous
versions of R (3.5, 3.4, 3.3, 3.2).

gargle and rlang are newly Imported.

## bigrquery 1.1.1

CRAN release: 2019-05-16

- Fix test failure with dbplyr 1.4.0.

- [`bq_field()`](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  can now pass `description` parameter which will be applied in
  [`bq_table_create()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  call ([@byapparov](https://github.com/byapparov),
  [\#272](https://github.com/r-dbi/bigrquery/issues/272)).

- [`bq_table_patch()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md) -
  allows to patch table ([@byapparov](https://github.com/byapparov),
  [\#253](https://github.com/r-dbi/bigrquery/issues/253)) with new
  schema.

## bigrquery 1.1.0

CRAN release: 2019-02-05

### Improved type support

- [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  and the
  [`DBI::dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html)
  method now has a `bigint` argument which governs how BigQuery integer
  columns are imported into R. As before, the default is
  `bigint = "integer"`. You can set `bigint = "integer64"` to import
  BigQuery integer columns as
  [`bit64::integer64`](https://rdrr.io/pkg/bit64/man/bit64-package.html)
  columns in R which allows for values outside the range of `integer`
  (`-2147483647` to `2147483647`)
  ([@rasmusab](https://github.com/rasmusab),
  [\#94](https://github.com/r-dbi/bigrquery/issues/94)).

- [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  now treats NUMERIC columns the same was as FLOAT columns
  ([@paulsendavidjay](https://github.com/paulsendavidjay),
  [\#282](https://github.com/r-dbi/bigrquery/issues/282)).

- [`bq_table_upload()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  works with POSIXct/POSIXct variables
  ([\#251](https://github.com/r-dbi/bigrquery/issues/251))

### SQL translation

- [`as.character()`](https://rdrr.io/r/base/character.html) now
  translated to `SAFE_CAST(x AS STRING)`
  ([\#268](https://github.com/r-dbi/bigrquery/issues/268)).

- [`median()`](https://rdrr.io/r/stats/median.html) now translates to
  `APPROX_QUANTILES(x, 2)[SAFE_ORDINAL(2)]`
  ([@valentinumbach](https://github.com/valentinumbach),
  [\#267](https://github.com/r-dbi/bigrquery/issues/267)).

### Minor bug fixes and improvements

- Jobs now print their ids while running
  ([\#252](https://github.com/r-dbi/bigrquery/issues/252))

- [`bq_job()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  tracks location so bigrquery now works painlessly with non-US/EU
  locations ([\#274](https://github.com/r-dbi/bigrquery/issues/274)).

- [`bq_perform_upload()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)
  will only autodetect a schema if the table does not already exist.

- [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  correctly computes page ranges if both `max_results` and `start_index`
  are supplied ([\#248](https://github.com/r-dbi/bigrquery/issues/248))

- Unparseable date times return NA
  ([\#285](https://github.com/r-dbi/bigrquery/issues/285))

## bigrquery 1.0.0

CRAN release: 2018-04-24

### Improved downloads

The system for downloading data from BigQuery into R has been rewritten
from the ground up to give considerable improvements in performance and
flexibility.

- The two steps, downloading and parsing, now happen in sequence, rather
  than interleaved. This means that you’ll now see two progress bars:
  one for downloading JSON from BigQuery and one for parsing that JSON
  into a data frame.

- Downloads now occur in parallel, using up to 6 simultaneous
  connections by default.

- The parsing code has been rewritten in C++. As well as considerably
  improving performance, this also adds support for nested
  (record/struct) and repeated (array) columns
  ([\#145](https://github.com/r-dbi/bigrquery/issues/145)). These
  columns will yield list-columns in the following forms:

  - Repeated values become list-columns containing vectors.
  - Nested values become list-columns containing named lists.
  - Repeated nested values become list-columns containing data frames.

- Results are now returned as tibbles, not data frames, because the base
  print method does not handle list columns well.

I can now download the first million rows of
`publicdata.samples.natality` in about a minute. This data frame is
about 170 MB in BigQuery and 140 MB in R; a minute to download this much
data seems reasonable to me. The bottleneck for loading BigQuery data is
now parsing BigQuery’s json format. I don’t see any obvious way to make
this faster as I’m already using the fastest C++ json parser,
[RapidJson](http://rapidjson.org). If this is still too slow for you
(i.e. you’re downloading GBs of data), see
[`?bq_table_download`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
for an alternative approach.

### New features

#### dplyr

- [`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html)
  now works ([@realAkhmed](https://github.com/realAkhmed),
  [\#52](https://github.com/r-dbi/bigrquery/issues/52)).

- [`tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) now accepts
  fully (or partially) qualified table names, like
  “publicdata.samples.shakespeare” or “samples.shakespeare”. This makes
  it possible to join tables across datasets
  ([\#219](https://github.com/r-dbi/bigrquery/issues/219)).

#### DBI

- [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) now
  defaults to standard SQL, rather than legacy SQL. Use
  `use_legacy_sql = TRUE` if you need the previous behaviour
  ([\#147](https://github.com/r-dbi/bigrquery/issues/147)).

- [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) now
  allows `dataset` to be omitted; this is natural when you want to use
  tables from multiple datasets.

- [`dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html)
  and
  [`dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html)
  now accept fully (or partially) qualified table names.

- [`dbi_driver()`](https://bigrquery.r-dbi.org/dev/reference/bigquery.md)
  is deprecated; please use
  [`bigquery()`](https://bigrquery.r-dbi.org/dev/reference/bigquery.md)
  instead.

#### Low-level API

The low-level API has been completely overhauled to make it easier to
use. The primary motivation was to make bigrquery development more
enjoyable for me, but it should also be helpful to you when you need to
go outside of the features provided by higher-level DBI and dplyr
interfaces. The old API has been soft-deprecated - it will continue to
work, but no further development will occur (including bug fixes). It
will be formally deprecated in the next version, and then removed in the
version after that.

- **Consistent naming scheme**: All API functions now have the form
  `bq_object_verb()`, e.g.
  [`bq_table_create()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md),
  or
  [`bq_dataset_delete()`](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md).

- **S3 classes**:
  [`bq_table()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md),
  [`bq_dataset()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md),
  [`bq_job()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md),
  [`bq_field()`](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  and
  [`bq_fields()`](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  constructor functions create S3 objects corresponding to important
  BigQuery objects
  ([\#150](https://github.com/r-dbi/bigrquery/issues/150)). These are
  paired with `as_` coercion functions and used throughout the new API.

- **Easier local testing**: New
  [`bq_test_project()`](https://bigrquery.r-dbi.org/dev/reference/bq_test_project.md)
  and
  [`bq_test_dataset()`](https://bigrquery.r-dbi.org/dev/reference/bq_test_project.md)
  make it easier to run bigrquery tests locally. To run the tests
  yourself, you need to create a BigQuery project, and then follow the
  instructions in
  [`?bq_test_project`](https://bigrquery.r-dbi.org/dev/reference/bq_test_project.md).

- **More efficient data transfer**: The new API makes extensive use of
  the `fields` query parameter, ensuring that functions only download
  data that they actually use
  ([\#153](https://github.com/r-dbi/bigrquery/issues/153)).

- **Tighter GCS connection**: New
  [`bq_table_load()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  loads data from a Google Cloud Storage URI, pairing with
  [`bq_table_save()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  which saves data to a GCS URI
  ([\#155](https://github.com/r-dbi/bigrquery/issues/155)).

### Bug fixes and minor improvements

#### dplyr

- The dplyr interface can work with literal SQL once more
  ([\#218](https://github.com/r-dbi/bigrquery/issues/218)).

- Improved SQL translation for
  [`pmax()`](https://rdrr.io/r/base/Extremes.html),
  [`pmin()`](https://rdrr.io/r/base/Extremes.html),
  [`sd()`](https://rdrr.io/r/stats/sd.html),
  [`all()`](https://rdrr.io/r/base/all.html), and
  [`any()`](https://rdrr.io/r/base/any.html)
  ([\#176](https://github.com/r-dbi/bigrquery/issues/176),
  [\#179](https://github.com/r-dbi/bigrquery/issues/179),
  [@jarodmeng](https://github.com/jarodmeng)). And for
  [`paste0()`](https://rdrr.io/r/base/paste.html),
  [`cor()`](https://rdrr.io/r/stats/cor.html) and
  [`cov()`](https://rdrr.io/r/stats/cor.html)
  ([@edgararuiz](https://github.com/edgararuiz)).

- If you have the development version of dbplyr installed,
  [`print()`](https://rdrr.io/r/base/print.html)ing a BigQuery table
  will not perform an unneeded query, but will instead download directly
  from the table
  ([\#226](https://github.com/r-dbi/bigrquery/issues/226)).

#### Low-level

- Request error messages now contain the “reason”, which can contain
  useful information for debugging
  ([\#209](https://github.com/r-dbi/bigrquery/issues/209)).

- [`bq_dataset_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)
  and
  [`bq_project_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)
  can now supply query parameters
  ([\#191](https://github.com/r-dbi/bigrquery/issues/191)).

- [`bq_table_create()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  can now specify `fields`
  ([\#204](https://github.com/r-dbi/bigrquery/issues/204)).

- [`bq_perform_query()`](https://bigrquery.r-dbi.org/dev/reference/api-perform.md)
  no longer fails with empty results
  ([@byapparov](https://github.com/byapparov),
  [\#206](https://github.com/r-dbi/bigrquery/issues/206)).

## Version 0.4.1

CRAN release: 2017-06-26

- Fix SQL translation omissions discovered by dbplyr 1.1.0

## Version 0.4.0

CRAN release: 2017-06-23

### New features

- dplyr support has been updated to require dplyr 0.7.0 and use dbplyr.
  This means that you can now more naturally work directly with DBI
  connections. dplyr now also uses modern BigQuery SQL which supports a
  broader set of translations. Along the way I’ve also fixed some SQL
  generation bugs
  ([\#48](https://github.com/r-dbi/bigrquery/issues/48)).

- The DBI driver gets a new name:
  [`bigquery()`](https://bigrquery.r-dbi.org/dev/reference/bigquery.md).

- New `insert_extract_job()` make it possible to extract data and save
  in google storage ([@realAkhmed](https://github.com/realAkhmed),
  [\#119](https://github.com/r-dbi/bigrquery/issues/119)).

- New `insert_table()` allows you to insert empty tables into a dataset.

- All POST requests (inserts, updates, copies and `query_exec`) now take
  `...`. This allows you to add arbitrary additional data to the request
  body making it possible to use parts of the BigQuery API that are
  otherwise not exposed
  ([\#149](https://github.com/r-dbi/bigrquery/issues/149)). `snake_case`
  argument names are automatically converted to `camelCase` so you can
  stick consistently to snake case in your R code.

- Full support for DATE, TIME, and DATETIME types
  ([\#128](https://github.com/r-dbi/bigrquery/issues/128)).

### Big fixes and minor improvements

- All bigrquery requests now have a custom user agent that specifies the
  versions of bigrquery and httr that are used
  ([\#151](https://github.com/r-dbi/bigrquery/issues/151)).

- [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) gains
  new `use_legacy_sql`, `page_size`, and `quiet` arguments that are
  passed onto `query_exec()`. These allow you to control query options
  at the connection level.

- `insert_upload_job()` now sends data in newline-delimited JSON instead
  of csv ([\#97](https://github.com/r-dbi/bigrquery/issues/97)). This
  should be considerably faster and avoids character encoding issues
  ([\#45](https://github.com/r-dbi/bigrquery/issues/45)). `POSIXlt`
  columns are now also correctly coerced to TIMESTAMPS
  ([\#98](https://github.com/r-dbi/bigrquery/issues/98)).

- `insert_query_job()` and `query_exec()` gain new arguments:

  - `quiet = TRUE` will suppress the progress bars if needed.
  - `use_legacy_sql = FALSE` option allows you to opt-out of the legacy
    SQL system ([\#124](https://github.com/r-dbi/bigrquery/issues/124),
    [@backlin](https://github.com/backlin))

- `list_tables()`
  ([\#108](https://github.com/r-dbi/bigrquery/issues/108)) and
  `list_datasets()`
  ([\#141](https://github.com/r-dbi/bigrquery/issues/141)) are now
  paginated. By default they retrieve 50 items per page, and will
  iterate until they get everything.

- `list_tabledata()` and `query_exec()` now give a nicer progress bar,
  including estimated time remaining
  ([\#100](https://github.com/r-dbi/bigrquery/issues/100)).

- `query_exec()` should be considerably faster because profiling
  revealed that ~40% of the time taken by was a single line inside a
  function that helps parse BigQuery’s json into an R data frame. I
  replaced the slow R code with a faster C function.

- `set_oauth2.0_cred()` allows user to supply their own Google OAuth
  application when setting credentials
  ([\#130](https://github.com/r-dbi/bigrquery/issues/130),
  [@jarodmeng](https://github.com/jarodmeng))

- `wait_for()` uses now reports the query total bytes billed, which is
  more accurate because it takes into account caching and other factors.

- `list_tabledata` returns empty table on max_pages=0
  ([\#184](https://github.com/r-dbi/bigrquery/issues/184),
  [@ras44](https://github.com/ras44)
  [@byapparov](https://github.com/byapparov))

## Version 0.3.0

CRAN release: 2016-06-28

- New `set_service_token()` allows you to use OAuth service token
  instead of interactive authentication.from

- `^` is correctly translated to `pow()`
  ([\#110](https://github.com/r-dbi/bigrquery/issues/110)).

- Provide full DBI compliant interface
  ([@krlmlr](https://github.com/krlmlr)).

- Backend now translates `iflese()` to `IF`
  ([@realAkhmed](https://github.com/realAkhmed),
  [\#53](https://github.com/r-dbi/bigrquery/issues/53)).

## Version 0.2.0.

CRAN release: 2016-03-03

- Compatible with latest httr.

- Computation of the SQL data type that corresponds to a given R object
  is now more robust against unknown classes.
  ([\#95](https://github.com/r-dbi/bigrquery/issues/95),
  [@krlmlr](https://github.com/krlmlr))

- A data frame with full schema information is returned for zero-row
  results. ([\#88](https://github.com/r-dbi/bigrquery/issues/88),
  [@krlmlr](https://github.com/krlmlr))

- New `exists_table()`.
  ([\#91](https://github.com/r-dbi/bigrquery/issues/91),
  [@krlmlr](https://github.com/krlmlr))

- New arguments `create_disposition` and `write_disposition` to
  `insert_upload_job()`.
  ([\#92](https://github.com/r-dbi/bigrquery/issues/92),
  [@krlmlr](https://github.com/krlmlr))

- Renamed option `bigquery.quiet` to `bigrquery.quiet`.
  ([\#89](https://github.com/r-dbi/bigrquery/issues/89),
  [@krlmlr](https://github.com/krlmlr))

- New `format_dataset()` and `format_table()`.
  ([\#81](https://github.com/r-dbi/bigrquery/issues/81),
  [@krlmlr](https://github.com/krlmlr))

- New `list_tabledata_iter()` that allows fetching a table in chunks of
  varying size. ([\#77](https://github.com/r-dbi/bigrquery/issues/77),
  [\#87](https://github.com/r-dbi/bigrquery/issues/87),
  [@krlmlr](https://github.com/krlmlr))

- Add support for API keys via the `BIGRQUERY_API_KEY` environment
  variable. ([\#49](https://github.com/r-dbi/bigrquery/issues/49))
