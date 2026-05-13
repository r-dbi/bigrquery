# A BigQuery data source for dplyr.

Create the connection to the database with
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
then use
[`dplyr::tbl()`](https://dplyr.tidyverse.org/reference/tbl.html) to
connect to tables within that database. Generally, it's best to provide
the fully qualified name of the table (i.e. `project.dataset.table`) but
if you supply a default `dataset` in the connection, you can use just
the table name. (This, however, will prevent you from making joins
across datasets.)

## Usage

``` r
src_bigquery(project, dataset, billing = project, max_pages = 10)
```

## Arguments

- project:

  project id or name

- dataset:

  dataset name

- billing:

  billing project, if different to `project`

- max_pages:

  (IGNORED) maximum pages returned by a query

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# To run this example, replace billing with the id of one of your projects
# set up for billing
con <- DBI::dbConnect(bigquery(), project = bq_test_project())

shakespeare <- con %>% tbl(I("publicdata.samples.shakespeare"))
shakespeare
shakespeare %>%
  group_by(word) %>%
  summarise(n = sum(word_count, na.rm = TRUE)) %>%
  arrange(desc(n))
} # }
```
