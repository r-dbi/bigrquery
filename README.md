# bigrquery

[![Build Status](https://travis-ci.org/rstats-db/bigrquery.svg?branch=master)](https://travis-ci.org/rstats-db/bigrquery)

The bigrquery packages provides an R interface to
[Google BigQuery](https://developers.google.com/bigquery/). It makes it easy
to retrieve metadata about your projects, datasets, tables and jobs, and
provides a convenient wrapper for working with bigquery from R.

## Installation

The current bigrquery release can be installed from CRAN: 

```R
install.packages("bigrquery")
```

The newest development release can be installed from github:

```R
# install.packages('devtools')
devtools::install_github("rstats-db/bigrquery")
```

## Authentication

The first time you use bigrquery in a session, it will ask you to
[authorize bigrquery](https://developers.google.com/bigquery/authorization) in
the browser. This gives bigrquery the credentials to access data on your
behalf. By default, bigrquery picks up [httr's](http://github.com/hadley/httr)
policy of caching per-working-directory credentials in `.httr-oauth`.

Note that `bigrquery` requests permission to modify your data; in general, the
only data created or modified by `bigrquery` are the temporary tables created
as query results, unless you explicitly modify your own data (say by calling
`delete_table()` or `insert_upload_job()`).

## Sample data and a billing project

If you just want to play around with the bigquery API, it's easiest to start
with the Google's free
[sample data](https://developers.google.com/bigquery/docs/sample-tables). To
do that, you'll also need to create your own project for billing purposes. If
you're just playing around, it's unlikely that you'll go over the 10,000
request/day free limit, but google still needs a project that it can bill (you
don't even need to provide a credit card).

To create a project:

1. Open https://console.cloud.google.com/
2. Click "Create Project" at the top
3. Select a name and project ID, and click "Create"
4. Turn on the BigQuery API by clicking "APIs & Auth" on the left, scrolling
down to "BigQuery API", and clicking the button at the right from "OFF" to
"ON".
5. Click on "Overview" at the left
6. Use the `Project ID` or `Project Number` to identify your project with
`bigrquery`. (You can also use the project number, though it's harder to
remember.)

To run your first query:

```R
library(bigrquery)
project <- "fantastic-voyage-389" # put your project ID here
sql <- "SELECT year, month, day, weight_pounds FROM [publicdata:samples.natality] LIMIT 5"
query_exec(sql, project = project)
```

## Useful links

* [SQL reference](https://developers.google.com/bigquery/query-reference)
* [API reference](https://developers.google.com/bigquery/docs/reference/v2/)
* [Query/job console](https://bigquery.cloud.google.com/)
* [Billing console](https://console.cloud.google.com/)
