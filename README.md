# bigrquery

The bigrquery provides a read-only interface to [Google's bigquery](https://developers.google.com/bigquery/). It makes it easy to retrieve metadata about your projects, datasets, tables and jobs, and provides a convenient wrapper for working with bigquery from R.

bigrquery is not currently available on CRAN, but you can install it with devtools:

```R
devtools::install_github("assertthat")
devtools::install_github("dplyr")
devtools::install_github("bigrquery")
```

## Authentication

The first time you use bigrquery in a session, it will ask you to [authorize bigrquery](https://developers.google.com/bigquery/authorization) in the browser. This gives bigrquery the credentials to access data on your behalf. The credentials are not stored on disk, and need to be re-entered every time you restart R.

Note that bigrquery asks for authorisation to modify your data, but it never actually does so. This permission is only necessary because bigquery queries create temporary tables that store the query output, and bigrquery needs appropriate permission to create them.

## Sample data and a billing project

If you just want to play around with the bigquery API, it's easiest to start with the Google's free [sample data](https://developers.google.com/bigquery/docs/sample-tables). To do that, you'll also need to create your own project for billing purposes. If you're just playing around, it's unlikely that you'll go over the 10,000 request/day free limit, but google still needs a project that it can bill (you don't even need to provide a credit card).

To create a project:

1. Open https://code.google.com/apis/
2. In the project drop-drop at the top-right, select "Create"
3. Give your project a name (it doesn't matter what)
4. Turn bigquery API access on 
5. Click on "overview" (under the project drop-down at the top-left)
6. Note your project number: this is the unique identifer for your project

```R
library(bigrquery)
billing_project <- "341409650721" # put your project number here
sql <- "SELECT year, month, day, weight_pounds FROM natality LIMIT 5"
query_exec("publicdata", "samples", sql, billing = billing_project)
```

## dplyr support

bigrquery also supports the dplyr methods, so you can create a bigquery data source and then interact with it in the same way that you interact with data frames, data tables and local databases:

```R
library(bigrquery)
billing <- "341409650721" # put your project number here
births <- source_bigquery("publicdata", "samples", "natality", ling)
dim(births)
colnames(births)

head(births)
```