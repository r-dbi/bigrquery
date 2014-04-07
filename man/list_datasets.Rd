\name{list_datasets}
\alias{list_datasets}
\title{List the datasets in a project}
\usage{
  list_datasets(project)
}
\arguments{
  \item{project}{The project name, a string}
}
\value{
  a character vector of dataset names
}
\description{
  List the datasets in a project.
}
\examples{
\donttest{
list_datasets("publicdata")
}
}
\seealso{
  Google API documentation:
  \url{https://developers.google.com/bigquery/docs/reference/v2/datasets/list}
}

