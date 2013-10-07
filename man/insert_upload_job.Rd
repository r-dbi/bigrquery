\name{insert_upload_job}
\alias{insert_upload_job}
\title{Upload data.}
\usage{
  insert_upload_job(project, dataset, table, values,
    billing = project)
}
\arguments{
  \item{table}{name of table to insert values into}

  \item{value}{data frame of data to upload}

  \item{project}{project name}

  \item{dataset}{dataset name}

  \item{billing}{project to bill to, if different to
  \code{project}}
}
\description{
  This sends all of the data inline in the HTTP request so
  is only suitable for relatively small datasets.
}
\examples{
\dontrun{
list_datasets("193487687779")
list_tables("193487687779", "houston")
job <- insert_upload_job("193487687779", "houston", "mtcars", mtcars)
wait_for(job)
list_tables("193487687779", "houston")
}
}
\seealso{
  Google API documentation:
  \url{https://developers.google.com/bigquery/loading-data-into-bigquery#loaddatapostrequest}
}

