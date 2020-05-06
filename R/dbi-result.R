#' @include dbi-connection.R
NULL

BigQueryResult <- function(conn, sql, ...) {
  if (is.null(conn@dataset)) {
    job <- bq_perform_query(sql,
      billing = conn@billing,
      quiet = conn@quiet,
      ...
    )
  } else {
    ds <- as_bq_dataset(conn)
    job <- bq_perform_query(sql,
      billing = conn@billing,
      default_dataset = ds,
      quiet = conn@quiet,
      ...
    )
  }

  bq_job_wait(job, quiet = conn@quiet)
  meta <- bq_job_meta(job, paste0(
    "configuration(query(destinationTable)),",
    "statistics(query(statementType,numDmlAffectedRows))"
  ))

  tb <- as_bq_table(meta$configuration$query$destinationTable)

  if (meta$statistics$query$statementType == "SELECT") {
    nrow <- bq_table_nrow(tb)
  } else {
    nrow <- 0
  }

  affected <- as.numeric(meta$statistics$query$numDmlAffectedRows %||% 0)

  res <- new(
    "BigQueryResult",
    bq_table = tb,
    statement = sql,
    nrow = nrow,
    affected = affected,
    page_size = conn@page_size,
    quiet = conn@quiet,
    cursor = cursor(nrow),
    bigint = conn@bigint
  )
  res
}

#' @rdname DBI
#' @export
setClass(
  "BigQueryResult",
  contains = "DBIResult",
  slots = list(
    bq_table = "bq_table",
    statement = "character",
    nrow = "numeric",
    affected = "numeric",
    page_size = "numeric",
    quiet = "logical",
    cursor = "list",
    bigint = "character"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "BigQueryResult",
  function(object) {
    cat_line(
      "<BigQueryResult>\n",
      "  Query: ", dbGetStatement(object), "\n",
      "  Has completed: ", dbHasCompleted(object), "\n",
      "  Rows fetched: ", dbGetRowCount(object)
    )
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "BigQueryResult",
  function(dbObj, ...) TRUE
)

#' @rdname DBI
#' @inheritParams DBI::dbClearResult
#' @export
setMethod(
  "dbClearResult", "BigQueryResult",
  function(res, ...) {
    invisible(TRUE)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbFetch
#' @export
setMethod(
  "dbFetch", "BigQueryResult",
  function(res, n = -1, ...) {
    stopifnot(length(n) == 1, is.numeric(n))
    stopifnot(n == round(n), !is.na(n), n >= -1)

    if (n == -1 || n == Inf) {
      n <- res@cursor$left()
    }

    data <- bq_table_download(res@bq_table,
      max_results = n,
      start_index = res@cursor$cur(),
      page_size = res@page_size,
      bigint = res@bigint
    )
    res@cursor$adv(n)

    data
  })

#' @rdname DBI
#' @inheritParams DBI::dbHasCompleted
#' @export
setMethod(
  "dbHasCompleted", "BigQueryResult",
  function(res, ...) {
    res@cursor$left() == 0
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetStatement
#' @export
setMethod(
  "dbGetStatement", "BigQueryResult",
  function(res, ...) {
    res@statement
  })

#' @rdname DBI
#' @inheritParams DBI::dbColumnInfo
#' @export
setMethod(
  "dbColumnInfo", "BigQueryResult",
  function(res, ...) {
    fields <- bq_table_fields(res@bq_table)

    data.frame(
      name = vapply(fields, function(x) x$name, character(1)),
      type = vapply(fields, function(x) x$type, character(1)),
      stringsAsFactors = FALSE
    )
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowCount
#' @export
setMethod(
  "dbGetRowCount", "BigQueryResult",
  function(res, ...) {
    res@cursor$cur()
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetRowsAffected
#' @export
setMethod(
  "dbGetRowsAffected", "BigQueryResult",
  function(res, ...) {
    res@affected
  })

#' @rdname DBI
#' @inheritParams DBI::dbBind
#' @export
setMethod(
  "dbBind", "BigQueryResult",
  function(res, params, ...) {
    testthat::skip("Not yet implemented: dbBind(Result)")
  })



cursor <- function(nrow) {
  pos <- 0

  list(
    cur = function() pos,
    adv = function(i) {
      pos <<- pos + i
    },
    left = function() nrow - pos
  )
}
