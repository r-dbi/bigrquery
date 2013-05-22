\name{get_table}
\alias{get_table}
\title{Retrieve table metadata}
\usage{
  get_table(project, dataset, table)
}
\arguments{
  \item{table}{name of the table}

  \item{project}{project name}

  \item{dataset}{dataset name}
}
\value{
  A table resource list, as described by
  \url{https://developers.google.com/bigquery/docs/reference/v2/tables}
}
\description{
  Retrieve table metadata
}
\examples{
\donttest{
get_table("publicdata", "samples", "natality")
}
}
\seealso{
  API documentation:
  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/get}

  Other tables: \code{\link{list_tables}}
}

