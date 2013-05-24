\name{get_access_cred}
\alias{get_access_cred}
\alias{set_access_cred}
\title{Get and set access credentials}
\usage{
  get_access_cred()

  set_access_cred(value)
}
\arguments{
  \item{value}{new access credentials, as returned by
  \code{\link[httr]{oauth2.0_token}}}
}
\description{
  Since the majority of bigquery API requests need to be
  authenticated bigrquery maintains package-wide OAuth
  authentication credentials in a private environment. In
  ordinary operation, you should never need to use these
  functions but they are provided in case you want to
  switch credentials mid-stream.
}
\section{API console}{
  To manage your google projects, use the API console:
  \url{https://code.google.com/apis/console}
}
\keyword{internal}

