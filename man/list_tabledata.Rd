\name{list_tabledata}
\alias{list_tabledata}
\title{Retrieve data from a table}
\usage{
  list_tabledata(project, dataset, table, max_pages = Inf)
}
\arguments{
  \item{table}{name of the table}

  \item{max_pages}{maximum number of pages to retrieve. Use
  \code{Inf} to retrieve the complete dataset.}

  \item{project}{project name}

  \item{dataset}{dataset name}
}
\description{
  Retrieve data from a table
}
\seealso{
  API documentation at
  \url{https://developers.google.com/bigquery/docs/reference/v2/tabledata/list}
}

