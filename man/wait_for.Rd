\name{wait_for}
\alias{wait_for}
\title{Wait for a job to complete, optionally printing updates}
\usage{
  wait_for(job, quiet = FALSE, pause = 0.25)
}
\arguments{
  \item{job}{job to wait for. Probably result of
  \code{\link{insert_query_job}} or
  \code{\link{insert_update_job}}}

  \item{quiet}{if \code{FALSE} print informative progress
  messages}

  \item{pause}{amount of time to wait between status
  requests}
}
\description{
  Wait for a job to complete, optionally printing updates
}

