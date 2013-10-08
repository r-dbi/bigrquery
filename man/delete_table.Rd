\name{delete_table}
\alias{delete_table}
\title{Delete a table.}
\usage{
  delete_table(project, dataset, table)
}
\arguments{
  \item{table}{name of the table}

  \item{project}{project name}

  \item{dataset}{dataset name}
}
\description{
  Delete a table.
}
\examples{
\donttest{
get_table("publicdata", "samples", "natality")
}
}
\seealso{
  API documentation:
  \url{https://developers.google.com/bigquery/docs/reference/v2/tables/delete}

  Other tables: \code{\link{get_table}},
  \code{\link{list_tables}}
}

