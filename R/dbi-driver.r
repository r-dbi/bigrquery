#' DBI methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package.
#' @name DBI
NULL

#' BigQuery DBI driver
#'
#' Creates a BigQuery DBI driver for use in [DBI::dbConnect()].
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
#' @param use_legacy_sql If `TRUE`, uses BigQuery legacy SQL.
#' @export
setMethod(
  "dbConnect", "BigQueryDriver",
  function(drv, project, dataset, billing = project, use_legacy_sql = TRUE, ...) {
    BigQueryConnection(
      project = project,
      dataset = dataset,
      billing = billing,
      use_legacy_sql = TRUE
    )
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


#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "BigQueryDriver",
  function(dbObj, obj, ...) {
    data_type(obj)
  }
)
