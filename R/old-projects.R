#' List projects [deprecated]
#'
#' Please use [bq_projects()] instead.
#'
#' @keywords internal
#' @export
list_projects <- function() {
  .Deprecated("bq_projects", package = "bigrquery")

  bq_projects()
}
