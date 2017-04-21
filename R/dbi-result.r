#' @include dbi-connection.r
NULL

BigQueryResult <- function(connection, statement) {
  res <- new(
    "BigQueryResult",
    connection = connection,
    statement = statement,
    .envir = new.env(parent = emptyenv())
  )

  res@.envir$open <- TRUE

  dest <- run_query_job(
    query = statement,
    project = connection@billing,
    destination_table = NULL,
    default_dataset = format_dataset(connection@project, connection@dataset),
    use_legacy_sql = connection@use_legacy_sql,
    quiet = connection@quiet
  )

  res@.envir$iter <- list_tabledata_iter(
    project = dest$projectId,
    dataset = dest$datasetId,
    table = dest$tableId
  )

  res
}

#' @rdname DBI
#' @export
setClass(
  "BigQueryResult",
  contains = "DBIResult",
  slots = list(
    connection = "BigQueryConnection",
    statement = "character",
    .envir = "environment"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "BigQueryResult",
  function(object) {
    cat("<BigQueryResult>\n",
        "  Query: ", dbGetStatement(object), "\n",
        "  Has completed: ", dbHasCompleted(object), "\n",
        "  Rows fetched: ", dbGetRowCount(object), "\n",
        sep = "")
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "BigQueryResult",
  function(dbObj, ...) {
    dbObj@.envir$open
  })

#' @rdname DBI
#' @inheritParams DBI::dbClearResult
#' @export
setMethod(
  "dbClearResult", "BigQueryResult",
  function(res, ...) {
    if (!dbIsValid(res)) {
      warning("Result already closed.", call. = FALSE)
    }

    res@.envir$open <- FALSE
    set_result(res@connection, NULL)

    invisible(TRUE)
  })

#' @rdname DBI
#' @inheritParams DBI::dbFetch
#' @export
setMethod(
  "dbFetch", "BigQueryResult",
  function(res, n = -1, ..., row.names = NA) {
    assert_result_valid(res)

    if (n < 0) n <- Inf

    data <- res@.envir$iter$next_paged(n, page_size = res@connection@page_size)

    DBI::sqlColumnToRownames(data, row.names = row.names)
  })

#' @rdname DBI
#' @inheritParams DBI::dbHasCompleted
#' @export
setMethod(
  "dbHasCompleted", "BigQueryResult",
  function(res, ...) {
    assert_result_valid(res)

    res@.envir$iter$is_complete()
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetStatement
#' @export
setMethod(
  "dbGetStatement", "BigQueryResult",
  function(res, ...) {
    assert_result_valid(res)
    res@statement
  })

#' @rdname DBI
#' @inheritParams DBI::dbColumnInfo
#' @export
setMethod(
  "dbColumnInfo", "BigQueryResult",
  function(res, ...) {
    schema <- res@.envir$iter$get_schema()

    data.frame(
      name = vapply(schema$fields, function(x) x$name, character(1)),
      type = vapply(schema$fields, function(x) x$type, character(1)),
      stringsAsFactors = FALSE
    )
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowCount
#' @export
setMethod(
  "dbGetRowCount", "BigQueryResult",
  function(res, ...) {
    assert_result_valid(res)
    res@.envir$iter$get_rows_fetched()
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowsAffected
#' @export
setMethod(
  "dbGetRowsAffected", "BigQueryResult",
  function(res, ...) {
    assert_result_valid(res)
    0L
  })

#' @rdname DBI
#' @inheritParams DBI::dbBind
#' @export
setMethod(
  "dbBind", "BigQueryResult",
  function(res, params, ...) {
    testthat::skip("Not yet implemented: dbBind(Result)")
  })

assert_result_valid <- function(res) {
  if (!dbIsValid(res)) {
    stop("Result has been already cleared.", call. = FALSE)
  }
}
