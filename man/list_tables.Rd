\name{list_tables}
\alias{list_tables}
\title{List available tables in dataset.}
\usage{
  list_tables(project, dataset)
}
\arguments{
  \item{project}{project name}

  \item{dataset}{dataset name}
}
\value{
  a character vector of table names
}
\description{
  List available tables in dataset.
}
\examples{
\donttest{
list_tables("publicdata", "samples")
}
}
\seealso{
  API documentation:
  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/list}

  Other table commands: \code{\link{delete_table}},
  \code{\link{get_table}}
}

