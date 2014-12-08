#' A bigquery data source.
#'
#' Use \code{src_bigquery} to connect to an existing bigquery dataset,
#' and \code{tbl} to connect to tables within that database.
#'
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to \code{project}
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To run this example, replace billing with the id of one of your projects
#' # with set up with billing
#' gh <- src_bigquery("githubarchive", "github", billing = "465736758727")
#' }
src_bigquery <- function(project, dataset, billing = project) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr is required to use src_bigquery", call. = FALSE)
  }

  assert_that(is.string(project), is.string(dataset), is.string(billing))

  if (!require("bigrquery")) {
    stop("bigrquery package required to connect to bigquery db", call. = FALSE)
  }

  con <- structure(list(project = project, dataset = dataset,
    billing = billing), class = "bigquery")
  dplyr::src_sql("bigquery", con)
}

#' @export
#' @importFrom dplyr src_desc
src_desc.src_bigquery <- function(x) {
  paste0("bigquery [", x$con$project, ":", x$con$dataset, "]")
}

#' @export
#' @importFrom dplyr db_list_tables
db_list_tables.bigquery <- function(con) {
  list_tables(con$project, con$dataset)
}
