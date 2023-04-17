
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bigrquery

<!-- badges: start -->

[![CRAN
Status](https://www.r-pkg.org/badges/version/bigrquery)](https://cran.r-project.org/package=bigrquery)
[![R-CMD-check](https://github.com/r-dbi/bigrquery/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-dbi/bigrquery/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/r-dbi/bigrquery/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-dbi/bigrquery?branch=main)
<!-- badges: end -->

The bigrquery package makes it easy to work with data stored in [Google
BigQuery](https://cloud.google.com/bigquery/docs) by allowing you to
query BigQuery tables and retrieve metadata about your projects,
datasets, tables, and jobs. The bigrquery package provides three levels
of abstraction on top of BigQuery:

- The low-level API provides thin wrappers over the underlying REST API.
  All the low-level functions start with `bq_`, and mostly have the form
  `bq_noun_verb()`. This level of abstraction is most appropriate if
  you’re familiar with the REST API and you want do something not
  supported in the higher-level APIs.

- The [DBI interface](https://www.r-dbi.org) wraps the low-level API and
  makes working with BigQuery like working with any other database
  system. This is most convenient layer if you want to execute SQL
  queries in BigQuery or upload smaller amounts (i.e. \<100 MB) of data.

- The [dplyr interface](https://dbplyr.tidyverse.org/) lets you treat
  BigQuery tables as if they are in-memory data frames. This is the most
  convenient layer if you don’t want to write SQL, but instead want
  dbplyr to write it for you.

## Installation

The current bigrquery release can be installed from CRAN:

``` r
install.packages("bigrquery")
```

The newest development release can be installed from GitHub:

``` r
#install.packages("pak")
pak::pak("r-dbi/bigrquery")
```

## Usage

### Low-level API

``` r
library(bigrquery)
billing <- bq_test_project() # replace this with your project ID 
sql <- "SELECT year, month, day, weight_pounds FROM `publicdata.samples.natality`"

tb <- bq_project_query(billing, sql)
bq_table_download(tb, n_max = 10)
#> # A tibble: 10 × 4
#>     year month   day weight_pounds
#>    <int> <int> <int>         <dbl>
#>  1  1969    10     7          7.56
#>  2  1969     5     9          6.62
#>  3  1969     2     6          2.00
#>  4  1969     1     8          8.44
#>  5  1969     6    23          9.81
#>  6  1969     7    31          7.19
#>  7  1969    11     6          7.50
#>  8  1969    12    19          7.50
#>  9  1969     2    17          7.05
#> 10  1969     5     3          8.50
```

### DBI

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
#>   Billing: gargle-169921

dbListTables(con)
#> [1] "github_nested"   "github_timeline" "gsod"            "natality"       
#> [5] "shakespeare"     "trigrams"        "wikipedia"

dbGetQuery(con, sql, n = 10)
#> # A tibble: 10 × 4
#>     year month   day weight_pounds
#>    <int> <int> <int>         <dbl>
#>  1  1969    10     7          7.56
#>  2  1969     5     9          6.62
#>  3  1969     2     6          2.00
#>  4  1969     1     8          8.44
#>  5  1969     6    23          9.81
#>  6  1969     7    31          7.19
#>  7  1969    11     6          7.50
#>  8  1969    12    19          7.50
#>  9  1969     2    17          7.05
#> 10  1969     5     3          8.50
```

### dplyr

``` r
library(dplyr)

natality <- tbl(con, "natality")
#> Warning: <BigQueryConnection> uses an old dbplyr interface
#> ℹ Please install a newer version of the package or contact the maintainer
#> This warning is displayed once every 8 hours.

natality %>%
  select(year, month, day, weight_pounds) %>% 
  head(10) %>%
  collect()
#> # A tibble: 10 × 4
#>     year month   day weight_pounds
#>    <int> <int> <int>         <dbl>
#>  1  2005     5    NA          7.56
#>  2  2005     6    NA          4.75
#>  3  2005    11    NA          7.37
#>  4  2005     6    NA          7.81
#>  5  2005     5    NA          3.69
#>  6  2005    10    NA          6.95
#>  7  2005    12    NA          8.44
#>  8  2005    10    NA          8.69
#>  9  2005    10    NA          7.63
#> 10  2005     7    NA          8.27
```

## Important details

### Authentication and authorization

When using bigrquery interactively, you’ll be prompted to [authorize
bigrquery](https://cloud.google.com/bigquery/docs/authorization) in the
browser. You’ll be asked if you want to cache tokens for reuse in future
sessions. For non-interactive usage, it is preferred to use a service
account token, if possible. More places to learn about auth:

- Help for
  [`bigrquery::bq_auth()`](https://bigrquery.r-dbi.org/reference/bq_auth.html).
- [How gargle gets
  tokens](https://gargle.r-lib.org/articles/how-gargle-gets-tokens.html).
  - bigrquery obtains a token with `gargle::token_fetch()`, which
    supports a variety of token flows. This article provides full
    details, such as how to take advantage of Application Default
    Credentials or service accounts on GCE VMs.
- [Non-interactive
  auth](https://gargle.r-lib.org/articles/non-interactive-auth.html).
  Explains how to set up a project when code must run without any user
  interaction.
- [How to get your own API
  credentials](https://gargle.r-lib.org/articles/get-api-credentials.html).
  Instructions for getting your own OAuth client or service account
  token.

Note that bigrquery requests permission to modify your data; but it will
never do so unless you explicitly request it (e.g. by calling
`bq_table_delete()` or `bq_table_upload()`). Our [Privacy
policy](https://www.tidyverse.org/google_privacy_policy) provides more
info.

### Billing project

If you just want to play around with the BigQuery API, it’s easiest to
start with Google’s free [sample
data](https://cloud.google.com/bigquery/public-data). You’ll still need
to create a project, but if you’re just playing around, it’s unlikely
that you’ll go over the free limit (1 TB of queries / 10 GB of storage).

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
  reference](https://cloud.google.com/bigquery/docs/reference/standard-sql/functions-and-operators)
- [API reference](https://cloud.google.com/bigquery/docs/reference/rest)
- [Query/job console](https://console.cloud.google.com/bigquery/)
- [Billing console](https://console.cloud.google.com/)

## Policies

Please note that the ‘bigrquery’ project is released with a [Contributor
Code of Conduct](https://bigrquery.r-dbi.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

[Privacy policy](https://www.tidyverse.org/google_privacy_policy)
