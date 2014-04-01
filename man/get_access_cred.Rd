\name{get_access_cred}
\alias{get_access_cred}
\alias{reset_access_cred}
\alias{set_access_cred}
\title{Get and set access credentials}
\usage{
  get_access_cred()

  set_access_cred(value)

  reset_access_cred()
}
\arguments{
  \item{value}{new access credentials, as returned by
  \code{\link[httr]{oauth2.0_token}}}
}
\description{
  These functions allow you to manually get or set the credentials used
  by \code{bigrquery}. In ordinary operation, you shouldn't need to call
  these functions directly.
}
\section{API console}{
  To manage your google projects, use the API console:
  \url{https://cloud.google.com/console}
}
\keyword{internal}

