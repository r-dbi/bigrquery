\name{source_bigquery}
\alias{arrange.source_bigquery}
\alias{filter.source_bigquery}
\alias{mutate.source_bigquery}
\alias{select.source_bigquery}
\alias{source_bigquery}
\alias{summarise.source_bigquery}
\title{A bigquery data source.}
\usage{
  source_bigquery(project, dataset, table,
    billing = project)

  \method{filter}{source_bigquery} (.data, ...)

  \method{arrange}{source_bigquery} (.data, ...)

  \method{select}{source_bigquery} (.data, ...)

  \method{summarise}{source_bigquery} (.data, ...,
    .max_pages = 10L, .page_size = 10000L)

  \method{mutate}{source_bigquery} (.data, ...,
    .max_pages = 10L, .page_size = 10000L)
}
\description{
  A bigquery data source.
}
\section{Caching}{
  The variable names and number of rows are cached on
  source creation, on the assumption that you're probably
  doing analysis on a table that's not changing as you run
  queries. If this is not correct, then the values of
  \code{dim} and \code{dimnames} may be out of date, but it
  shouldn't otherwise affect operation.
}
\examples{
library(dplyr)
billing <- "341409650721" # put your project number here
births <- source_bigquery("publicdata", "samples", "natality", billing)
dim(births)
colnames(births)

head(births)

summarise(births, first_year = min(year), last_year = max(year))
date_info <- select(births, year:wday)
head(date_info)

head(filter(select(births, year:wday), year > 2000))
}

