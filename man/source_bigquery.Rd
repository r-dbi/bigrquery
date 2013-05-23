\name{source_bigquery}
\alias{source_bigquery}
\title{A bigquery data source.}
\usage{
  source_bigquery(project, dataset, table,
    billing = project)
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
billing <- "341409650721" # put your project number here
births <- source_bigquery("publicdata", "samples", "natality", billing)
dim(births)
colnames(births)

head(births)
}

