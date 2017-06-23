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
#' DBI::dbConnect(bigquery(), dataset = "mydb", project = "myproject")
#' }
dbi_driver <- function() {
  new("BigQueryDriver")
}

#' @export
#' @rdname dbi_driver
bigquery <- dbi_driver

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
#' @inheritParams query_exec
#' @export
setMethod(
  "dbConnect", "BigQueryDriver",
  function(drv, project, dataset, billing = project,
           page_size = 1e4,
           quiet = NA,
           use_legacy_sql = TRUE,
           ...) {
    BigQueryConnection(
      project = project,
      dataset = dataset,
      billing = billing,
      page_size = page_size,
      quiet = quiet,
      use_legacy_sql = use_legacy_sql
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
