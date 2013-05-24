\name{grouped_bigquery}
\alias{arrange.grouped_bigquery}
\alias{filter.grouped_bigquery}
\alias{group_by.source_bigquery}
\alias{grouped_bigquery}
\alias{mutate.grouped_bigquery}
\alias{select.grouped_bigquery}
\alias{summarise.grouped_bigquery}
\title{A grouped bigquery database.}
\usage{
  grouped_bigquery(source, group_by)

  \method{group_by}{source_bigquery} (x, ...)

  \method{filter}{grouped_bigquery} (.data, ...)

  \method{summarise}{grouped_bigquery} (.data, ...,
    .max_pages = 10L, .page_size = 10000L)

  \method{mutate}{grouped_bigquery} (.data, ...,
    .n = 1e+05)

  \method{arrange}{grouped_bigquery} (.data, ...)

  \method{select}{grouped_bigquery} (.data, ...)
}
\arguments{
  \item{source}{a data source or data frame.}

  \item{group_by}{\code{vars} partially evaluated in the
  correct environment}

  \item{x}{an existing bigquery data source}

  \item{...}{expressions describing how to group data}
}
\description{
  Typically you will create a grouped data table is to call
  the \code{group_by} method on a bigquery data source:
  this will take care of capturing the unevalated
  expressions for you.
}
\examples{
billing <- "341409650721" # put your project number here
births <- source_bigquery("publicdata", "samples", "natality", billing)
by_year_sex <- group_by(births, year, is_male)
wt <- summarise(by_year_sex, n = count(), wt = mean(weight_pounds))
wtdf <- as.data.frame(wt)
if (require("ggplot2")) {
qplot(year, wt, data = wtdf, geom = "line", colour = is_male)
}
}

