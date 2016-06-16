#' DBI methods
#'
#' Implementations of pure virtual functions defined in the \code{DBI} package.
#' @name DBI
NULL

#' BigQuery DBI driver
#'
#' Creates a BigQuery DBI driver for use in \code{\link[DBI]{dbConnect}}.
#'
#' @export
#' @import methods DBI
#' @examples
#' \dontrun{
#' #' library(DBI)
#' dbConnect(dbi_driver(), database = "mydb", project = "myproject")
#' }
dbi_driver <- function() {
  new("BigQueryDriver")
}

#' @rdname DBI
#' @export
setClass("BigQueryDriver", contains = "DBIDriver")

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "BigQueryDriver",
  function(object) {
    cat("<BigQueryDriver>\n")
  })

#' @rdname DBI
#' @inheritParams DBI::dbConnect
#' @inheritParams insert_upload_job
#' @export
setMethod(
  "dbConnect", "BigQueryDriver",
  function(drv, project, dataset, billing = project, ...) {
    BigQueryConnection(project = project, dataset = dataset, billing = billing)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "BigQueryDriver",
  function(dbObj, ...) {
    TRUE
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "BigQueryDriver",
  function(dbObj, ...) {
    list(
      driver.version = PACKAGE_VERSION,
      client.version = NA,
      max.connections = NA
    )
  })
