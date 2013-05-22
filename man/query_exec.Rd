\name{query_exec}
\alias{query_exec}
\title{Run a asynchronous query and retrieve results.}
\usage{
  query_exec(project, dataset, query, billing = project)
}
\arguments{
  \item{project}{project name}

  \item{dataset}{dataset name}

  \item{query}{SQL query string}

  \item{billing}{project to bill to, if different to
  \code{project}}
}
\description{
  This is a high-level function that inserts a query job
  (with \code{\link{insert_query_job}}), repeatedly checks
  the status (with \code{\link{get_job}}) until it is
  complete, then retrieves the results (with
  \code{\link{list_tabledata}})
}
\examples{
\dontrun{
sql <- "SELECT year, avg(weight_pounds) FROM natality WHERE year > 2005
  GROUP BY year"
query_exec("publicdata", "samples", sql, billing = "yourproject")
}
}
\seealso{
  Google documentation describing asynchronous queries:
  \url{https://developers.google.com/bigquery/docs/queries#asyncqueries}
}

