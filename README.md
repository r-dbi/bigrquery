
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bigrquery

[![Build
Status](https://travis-ci.org/r-dbi/bigrquery.svg?branch=master)](https://travis-ci.org/r-dbi/bigrquery)
[![CRAN
Status](https://www.r-pkg.org/badges/version/bigrquery)](https://cran.r-project.org/package=bigrquery)
[![Coverage
status](https://codecov.io/gh/r-dbi/bigrquery/branch/master/graph/badge.svg)](https://codecov.io/github/r-dbi/bigrquery?branch=master)

The bigrquery package makes it easy to work with data stored in [Google
BigQuery](https://developers.google.com/bigquery/) by allowing you to
query BigQuery tables and retrieve metadata about your projects,
datasets, tables, and jobs. The bigrquery package provides three levels
of abstraction on top of BigQuery:

  - The low-level API provides thin wrappers over the underlying REST
    API. All the low-level functions start with `bq_`, and mostly have
    the form `bq_noun_verb()`. This level of abstraction is most
    appropriate if you’re familiar with the REST API and you want do
    something not supported in the higher-level APIs.

  - The [DBI interface](http://www.r-dbi.org) wraps the low-level API
    and makes working with BigQuery like working with any other database
    system. This is most convenient layer if you want to execute SQL
    queries in BigQuery or upload smaller amounts (i.e. \<100 MB) of
    data.

  - The [dplyr interface](http://dbplyr.tidyverse.org/) lets you treat
    BigQuery tables as if they are in-memory data frames. This is the
    most convenient layer if you don’t want to write SQL, but instead
    want dbplyr to write it for you.

## Installation

The current bigrquery release can be installed from CRAN:

``` r
install.packages("bigrquery")
```

The newest development release can be installed from GitHub:

``` r
# install.packages('devtools')
devtools::install_github("r-dbi/bigrquery")
```

## Usage

### Low-level API

``` r
library(bigrquery)
billing <- bq_test_project() # replace this with your project ID 
sql <- "SELECT year, month, day, weight_pounds FROM `publicdata.samples.natality`"

tb <- bq_project_query(billing, sql)
#> Auto-refreshing stale OAuth token.
bq_table_download(tb, max_results = 10)
#> # A tibble: 10 x 4
#>     year month   day weight_pounds
#>    <int> <int> <int>         <dbl>
#>  1  1969     1    20          6.44
#>  2  1969     1     9          6.38
#>  3  1969     1     9          7.19
#>  4  1969     1    11          8.13
#>  5  1969     1     3          7.25
#>  6  1969     1    15          5.06
#>  7  1969     1    25         NA   
#>  8  1969     1     4          7.06
#>  9  1969     1     6          7.19
#> 10  1969     1    26          3.53
```

## DBI

``` r
library(DBI)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "publicdata",
  dataset = "samples",
  billing = billing
)
con 
#> <BigQueryConnection>
#>   Dataset: publicdata.samples
#>   Billing: bigrquery-examples

dbListTables(con)
#> [1] "github_nested"   "github_timeline" "gsod"            "natality"       
#> [5] "shakespeare"     "trigrams"        "wikipedia"

dbGetQuery(con, sql, n = 10)
#> # A tibble: 10 x 4
#>     year month   day weight_pounds
#>    <int> <int> <int>         <dbl>
#>  1  1969     1    20          6.44
#>  2  1969     1     9          6.38
#>  3  1969     1     9          7.19
#>  4  1969     1    11          8.13
#>  5  1969     1     3          7.25
#>  6  1969     1    15          5.06
#>  7  1969     1    25         NA   
#>  8  1969     1     4          7.06
#>  9  1969     1     6          7.19
#> 10  1969     1    26          3.53
```

### dplyr

``` r
library(dplyr)

natality <- tbl(con, "natality")

natality %>%
  select(year, month, day, weight_pounds) %>% 
  head(10) %>%
  collect()
#> # A tibble: 10 x 4
#>     year month   day weight_pounds
#>    <int> <int> <int>         <dbl>
#>  1  1969    11    29          6.00
#>  2  1969     2     6          8.94
#>  3  1969     5    16          6.88
#>  4  1970     9     4          7.13
#>  5  1970     1    24          7.63
#>  6  1970     6     6          9.00
#>  7  1970    10    30          6.50
#>  8  1971     3    18          5.75
#>  9  1971     8    11          6.19
#> 10  1971     1    23          5.75
```

## Important details

### Authentication

When using bigquery interactively, you’ll be prompted to [authorize
bigrquery](https://developers.google.com/bigquery/authorization) in the
browser. Your credentials will be cached across sessions in
`.httr-oauth`. For non-interactive usage, you’ll need to download a
service token JSON file and use `set_service_token()`.

Note that `bigrquery` requests permission to modify your data; but it
will never do so unless you explicitly request it (e.g. by calling
`bq_table_delete()` or `bq_table_upload()`).

### Billing project

If you just want to play around with the bigquery API, it’s easiest to
start with the Google’s free [sample
data](https://developers.google.com/bigquery/docs/sample-tables). You’ll
still need to create a project, but if you’re just playing around, it’s
unlikely that you’ll go over the free limit (1 TB of queries / 10 GB of
storage).

To create a project:

1.  Open <https://console.cloud.google.com/> and create a project. Make
    a note of the “Project ID” in the “Project info” box.

2.  Click on “APIs & Services”, then “Dashboard” in the left the left
    menu.

3.  Click on “Enable Apis and Services” at the top of the page, then
    search for “BigQuery API” and “Cloud storage”.

Use your project ID as the `billing` project whenever you work with free
sample data; and as the `project` when you work with your own data.

## Useful links

  - [SQL
    reference](https://developers.google.com/bigquery/query-reference)
  - [API
    reference](https://developers.google.com/bigquery/docs/reference/v2/)
  - [Query/job console](https://bigquery.cloud.google.com/)
  - [Billing console](https://console.cloud.google.com/)
