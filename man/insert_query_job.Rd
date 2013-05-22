\name{insert_query_job}
\alias{insert_query_job}
\title{Create a new query job.}
\usage{
  insert_query_job(project, dataset, query,
    billing = project)
}
\arguments{
  \item{project}{project name}

  \item{dataset}{dataset name}

  \item{query}{SQL query string}

  \item{billing}{project to bill to, if different to
  \code{project}}
}
\value{
  a job resource list, as documented at
  \url{https://developers.google.com/bigquery/docs/reference/v2/jobs}
}
\description{
  This is a low-level function that creates a query job. To
  wait until it is finished and then retrieve the results,
  see \code{\link{query_exec}}
}
\seealso{
  API documentation for insert method:
  \url{https://developers.google.com/bigquery/docs/reference/v2/jobs/insert}
}

