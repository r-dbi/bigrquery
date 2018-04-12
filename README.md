
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bigrquery

[![Build
Status](https://travis-ci.org/r-dbi/bigrquery.svg?branch=master)](https://travis-ci.org/r-dbi/bigrquery)
[![CRAN
Status](https://www.r-pkg.org/badges/version/bigrquery)](https://cran.r-project.org/package=bigrquery)
[![Coverage
status](https://codecov.io/gh/r-dbi/bigrquery/branch/master/graph/badge.svg)](https://codecov.io/github/r-dbi/bigrquery?branch=master)

The bigrquery packages provides an interface to [Google
BigQuery](https://developers.google.com/bigquery/) from R. It makes it
easy to retrieve metadata about your projects, datasets, tables and
jobs, and provides a convenient wrapper for working with bigquery from
R.

## Installation

The current bigrquery release can be installed from CRAN:

``` r
install.packages("bigrquery")
```

The newest development release can be installed from github:

``` r
# install.packages('devtools')
devtools::install_github("r-dbi/bigrquery")
```

## Authentication

The first time you use bigrquery in a session, it will ask you to
[authorize
bigrquery](https://developers.google.com/bigquery/authorization) in the
browser. This gives bigrquery the credentials to access data on your
behalf. By default, bigrquery picks up
[httr’s](http://github.com/hadley/httr) policy of caching
per-working-directory credentials in `.httr-oauth`.

Note that `bigrquery` requests permission to modify your data; in
general, the only data created or modified by `bigrquery` are the
temporary tables created as query results, unless you explicitly modify
your own data (say by calling `bq_table_delete()` or
`bq_table_upload()`).

## Sample data and a billing project

If you just want to play around with the bigquery API, it’s easiest to
start with the Google’s free [sample
data](https://developers.google.com/bigquery/docs/sample-tables). To do
that, you’ll also need to create your own project for billing purposes.
If you’re just playing around, it’s unlikely that you’ll go over the
10,000 request/day free limit, but google still needs a project that it
can bill (you don’t even need to provide a credit card).

To create a project:

1.  Open <https://console.cloud.google.com/> and create a project. Make
    a note of the “Project ID” in the “Project info” box.

2.  Click on “APIs & Services”, then “Dashboard” in the left the left
    menu.

3.  Click on “Enable Apis and Services” at the top of the page, then
    search for “BigQuery API” and “Cloud storage”.

## Layers

### Low-level API

To run your first query:

``` r
library(bigrquery)
billing <- bq_test_project() # replace this with your project ID 
sql <- "SELECT year, month, day, weight_pounds FROM `publicdata.samples.natality` LIMIT 5"
bq_project_query(billing, sql)
#> <bq_table> bigrquery-examples._c28cdd32b5e04f92fed49d69a69f7d352d42fc24.anon37894304c386f62d096229a2daa12192d022a08f
```

## DBI

For a more traditional database connection using `dbConnect()`, the
driver to use is `bigrquery::dbi_driver()`. You will need to supply two
arguments: `project` and `dataset`. If this is a public data, you’ll
also need to specific your own `billing` project for google to charge.

``` r
con <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "noaa_gsod",
  billing = bq_test_project()
)
con 
#> <BigQueryConnection>
#>   Dataset: bigquery-public-data.noaa_gsod
#>   Billing: bigrquery-examples

head(dbListTables(con))
#> [1] "gsod1929" "gsod1930" "gsod1931" "gsod1932" "gsod1933" "gsod1934"
```

### dplyr

You can also use this connection with dplyr. The `stations` table can
then be referred to using `tbl()`

``` r
dplyr::tbl(con, "stations")
#> # Source:   table<stations> [?? x 11]
#> # Database: BigQueryConnection
#>    usaf   wban  name    country state call    lat    lon elev  begin end  
#>    <chr>  <chr> <chr>   <chr>   <chr> <chr> <dbl>  <dbl> <chr> <chr> <chr>
#>  1 007026 99999 WXPOD … AF      ""    ""      0.    0.   +702… 2014… 2017…
#>  2 007070 99999 WXPOD … AF      ""    ""      0.    0.   +707… 2014… 2015…
#>  3 010015 99999 BRINGE… NO      ""    ""     61.4   5.87 +032… 1987… 2011…
#>  4 010016 99999 RORVIK… NO      ""    ""     64.8  11.2  +001… 1987… 1991…
#>  5 010060 99999 EDGEOYA NO      ""    ""     78.2  22.8  +001… 1973… 2018…
#>  6 010050 99999 ISFJOR… SV      ""    ""     78.1  13.6  +000… 1931… 2014…
#>  7 010070 99999 NY-ALE… SV      ""    ""     78.9  11.9  +000… 1973… 2018…
#>  8 010017 99999 FRIGG   NO      ""    ENFR   60.0   2.25 +004… 1988… 2005…
#>  9 010010 99999 JAN MA… NO      ""    ENJA   70.9  -8.67 +000… 1931… 2018…
#> 10 010014 99999 SORSTO… NO      ""    ENSO   59.8   5.34 +004… 1986… 2018…
#> # ... with more rows
```

## Useful links

  - [SQL
    reference](https://developers.google.com/bigquery/query-reference)
  - [API
    reference](https://developers.google.com/bigquery/docs/reference/v2/)
  - [Query/job console](https://bigquery.cloud.google.com/)
  - [Billing console](https://console.cloud.google.com/)
