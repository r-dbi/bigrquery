#' DBI methods
#'
#' Implementations of pure virtual functions defined in the `DBI` package.
#' @name DBI
#' @keywords internal
NULL

#' BigQuery DBI driver
#'
#' Creates a BigQuery DBI driver for use in [DBI::dbConnect()].
#'
#' @export
#' @usage NULL
#' @import methods DBI
#' @examples
#' if (bq_testable()) {
#' con <- DBI::dbConnect(
#'   bigquery(),
#'   project = "publicdata",
#'   dataset = "samples",
#'   billing = bq_test_project()
#' )
#' con
#' DBI::dbListTables(con)
#' DBI::dbReadTable(con, "natality", max_results =10)
#'
#' # Create a temporary dataset to explore
#' ds <- bq_test_dataset()
#' con <- DBI::dbConnect(
#'   bigquery(),
#'   project = ds$project,
#'   dataset = ds$dataset
#' )
#' DBI::dbWriteTable(con, "mtcars", mtcars)
#' DBI::dbReadTable(con, "mtcars")[1:6, ]
#'
#' DBI::dbGetQuery(con, "SELECT count(*) FROM mtcars")
#'
#' res <- DBI::dbSendQuery(con, "SELECT cyl, mpg FROM mtcars")
#' dbColumnInfo(res)
#' dbFetch(res, 10)
#' dbFetch(res, -1)
#' DBI::dbHasCompleted(res)
#'
#' }
bigquery <- function() {
  new("BigQueryDriver")
}

#' @export
#' @rdname bigquery
#' @usage NULL
dbi_driver  <- function() {
  warning(
    "`dbi_driver()` deprecated; please use `bigquery()` instead`",
    call. = FALSE
  )
  new("BigQueryDriver")
}

#' @rdname DBI
#' @export
setClass("BigQueryDriver", contains = "DBIDriver")

#' @rdname bigquery
#' @inheritParams DBI::dbConnect
#' @param project,dataset Project and dataset identifiers
#' @inheritParams bq_perform_query
#' @inheritParams bq_projects
#' @inheritParams api-job
#' @param bigint The R type that BigQuery's 64-bit integer types should be mapped to.
#'   The default is `"integer"` which returns R's `integer` type but results in `NA` for
#'   values above/below +/- 2147483647. `"integer64"` returns a [bit64::integer64],
#'   which allows the full range of 64 bit integers.
#' @param ... Other arguments for compatbility with generic; currently ignored.
#' @export
setMethod(
  "dbConnect", "BigQueryDriver",
  function(drv, project, dataset = NULL, billing = project,
           page_size = 1e4,
           quiet = NA,
           use_legacy_sql = FALSE,
           bigint = c("integer", "integer64", "numeric", "character"),
           ...) {
    BigQueryConnection(
      project = project,
      dataset = dataset,
      billing = billing,
      page_size = page_size,
      quiet = quiet,
      use_legacy_sql = use_legacy_sql,
      bigint = match.arg(bigint)
    )
  }
)


#' @rdname DBI
#' @export
setMethod(
  "dbConnect", "bq_dataset",
  function(drv, ...) {
    DBI::dbConnect(bigquery(),
      project = drv$project,
      dataset = drv$dataset,
      ...
    )
  }
)


# Included for DBI compatibility ------------------------------------------
# nocov start
#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "BigQueryDriver",
  function(object) {
    cat("<BigQueryDriver>\n")
  }
)

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
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "BigQueryDriver",
  function(dbObj, ...) {
    TRUE
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "BigQueryDriver",
  function(dbObj, obj, ...) {
    data_type(obj)
  }
)
# nocov end
