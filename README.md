
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
bq_table_download(tb, max_results = 10)
#> # A tibble: 10 x 4
#>     year month   day weight_pounds
#>    <int> <int> <int>         <dbl>
#>  1  1969     2    18          7.56
#>  2  1969     6     5          5.88
#>  3  1969     6    15          8.00
#>  4  1969     3     2          8.50
#>  5  1969     7     6          7.31
#>  6  1969     8    16          7.69
#>  7  1969     3    22          8.44
#>  8  1969     2    12          6.88
#>  9  1969     4     2          7.94
#> 10  1969     7    30          8.50
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
#>  1  1969     2    18          7.56
#>  2  1969     6     5          5.88
#>  3  1969     6    15          8.00
#>  4  1969     3     2          8.50
#>  5  1969     7     6          7.31
#>  6  1969     8    16          7.69
#>  7  1969     3    22          8.44
#>  8  1969     2    12          6.88
#>  9  1969     4     2          7.94
#> 10  1969     7    30          8.50
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
#>  1  1969     3    12          5.81
#>  2  1969     2    18          7.23
#>  3  1969     8    22          7.06
#>  4  1970     4     1          8.56
#>  5  1970     2    20          7.87
#>  6  1970     6    22          6.69
#>  7  1970     4    27          7.50
#>  8  1970     6    21          4.81
#>  9  1969     7     9          6.62
#> 10  1969     8    16          8.44
```

## Important details

### Authentication

When using bigquery interactively, you’ll be prompted to [authorize
bigrquery](https://developers.google.com/bigquery/authorization) in the
browser. Your token will be cached across sessions in
`~/.R/gargle/gargle-oauth`, by default. For non-interactive usage, you
can download a service account token as a JSON file and put it into
force via `bq_auth(path = "/path/to/your/service-account.json")`.
bigrquery obtains a token with `gargle::token_fetch()`, which supports
even more token flows. For full details, such as how to take advantage
of Application Default Credentials or service accounts on GCE VMs, read
the gargle vignette [How gargle gets
tokens](https://gargle.r-lib.org/articles/how-gargle-gets-tokens.html).

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

## Policies

Please note that the ‘bigrquery’ project is released with a [Contributor
Code of Conduct](https://bigrquery.r-dbi.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

[Privacy policy](https://www.tidyverse.org/google_privacy_policy)
