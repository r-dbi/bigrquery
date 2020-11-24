#' @include dbi-driver.R
NULL

BigQueryConnection <-
  function(project, dataset, billing,
           page_size = 1e4,
           quiet = NA,
           use_legacy_sql = FALSE,
           bigint = c("integer", "integer64", "numeric", "character")) {
  ret <- new("BigQueryConnection",
    project = project,
    dataset = dataset,
    billing = billing,
    page_size = as.integer(page_size),
    quiet = quiet,
    use_legacy_sql = use_legacy_sql,
    bigint = match.arg(bigint)
  )
  ret
}

#' @rdname DBI
#' @export
setClass(
  "BigQueryConnection",
  contains = "DBIConnection",
  slots = list(
    project = "character",
    dataset = "ANY",
    billing = "character",
    use_legacy_sql = "logical",
    page_size = "integer",
    quiet = "logical",
    bigint = "character"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "BigQueryConnection",
  function(object) {
    cat_line("<BigQueryConnection>")

    if (!is.null(object@dataset)) {
      cat_line("  Dataset: ", object@project, ".", object@dataset)
    }
    cat_line("  Billing: ", object@billing)

  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "BigQueryConnection",
  function(dbObj, ...) {
    TRUE
  })

#' @rdname DBI
#' @inheritParams DBI::dbDisconnect
#' @export
setMethod(
  "dbDisconnect", "BigQueryConnection",
  function(conn, ...) {
    invisible(TRUE)
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
#' @export
setMethod(
  "dbSendQuery", c("BigQueryConnection", "character"),
  function(conn, statement, ...) {
    BigQueryResult(conn, statement, ...)
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteString
#' @export
setMethod(
  "dbQuoteString", c("BigQueryConnection", "character"),
  function(conn, x, ...) {
    if (length(x) == 0) {
      return(SQL(character()))
    }

    out <- encodeString(x, na.encode = FALSE, quote = "'")
    out[is.na(x)] <- "NULL"
    SQL(out, names = names(x))
  })

#' @rdname DBI
#' @export
setMethod(
  "dbQuoteString", c("BigQueryConnection", "SQL"),
  function(conn, x, ...) {
    x
  })

#' @rdname DBI
#' @inheritParams DBI::dbQuoteIdentifier
#' @export
setMethod(
  "dbQuoteIdentifier", c("BigQueryConnection", "character"),
  function(conn, x, ...) {
    if (length(x) == 0) {
      return(SQL(character()))
    }

    if (any(is.na(x))) {
      stop("Identifiers must not be missing", call. = FALSE)
    }

    if (conn@use_legacy_sql) {
      out <- paste0("[", x, "]")
    } else {
      out <- encodeString(x, quote = "`")
    }

    SQL(out, names = names(x))
  })

#' @rdname DBI
#' @export
setMethod(
  "dbQuoteIdentifier", c("BigQueryConnection", "SQL"),
  function(conn, x, ...) {
    x
  }
)


#' @rdname DBI
#' @inheritParams DBI::dbDataType
#' @export
setMethod(
  "dbDataType", "BigQueryConnection",
  function(dbObj, obj, ...) {
    data_type(obj)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbWriteTable
#' @param row.names A logical specifying whether the `row.names` should be
#'   output to the output DBMS table; if `TRUE`, an extra field whose name
#'   will be whatever the R identifier `"row.names"` maps to the DBMS (see
#'   [DBI::make.db.names()]). If `NA` will add rows names if
#'   they are characters, otherwise will ignore.
#' @param overwrite a logical specifying whether to overwrite an existing table
#'   or not. Its default is `FALSE`.
#' @param append a logical specifying whether to append to an existing table
#'   in the DBMS.  Its default is `FALSE`.
#' @param field.types,temporary Ignored. Included for compatibility with
#'   generic.
#' @export
setMethod(
  "dbWriteTable", c("BigQueryConnection", "character", "data.frame"),
  function(conn, name, value,
           overwrite = FALSE,
           append = FALSE,
           ...,
           field.types = NULL,
           temporary = FALSE,
           row.names = NA) {
    assert_that(is.flag(overwrite), is.flag(append))

    if (!is.null(field.types)) {
      stop("`field.types` not supported by bigrquery", call. = FALSE)
    }
    if (!identical(temporary, FALSE)) {
      stop("Temporary tables not supported by bigrquery", call. = FALSE)
    }

    if (append) {
      create_disposition <- "CREATE_NEVER"
      write_disposition <- "WRITE_APPEND"
    } else {
      create_disposition <- "CREATE_IF_NEEDED"
      write_disposition <- if (overwrite) "WRITE_TRUNCATE" else "WRITE_EMPTY"
    }
    tb <- as_bq_table(conn, name)

    bq_table_upload(tb, value,
      create_disposition = create_disposition,
      write_disposition = write_disposition,
      ...
    )
    invisible(TRUE)
  })

#' @rdname DBI
#' @inheritParams DBI::dbReadTable
#' @export
setMethod(
  "dbReadTable", c("BigQueryConnection", "character"),
  function(conn, name, ...) {
    tb <- as_bq_table(conn, name)
    bq_table_download(tb, ...)
  })

#' @rdname DBI
#' @inheritParams DBI::dbListTables
#' @export
setMethod(
  "dbListTables", "BigQueryConnection",
  function(conn, ...) {
    if (is.null(conn@dataset)) {
      stop("To list table, must supply `dataset` when creating connection", call. = FALSE)
    }
    ds <- bq_dataset(conn@project, conn@dataset)

    tbs <- bq_dataset_tables(ds, ...)
    map_chr(tbs, function(x) x$table)
  })

#' @rdname DBI
#' @inheritParams DBI::dbExistsTable
#' @export
setMethod(
  "dbExistsTable", c("BigQueryConnection", "character"),
  function(conn, name, ...) {
    tb <- as_bq_table(conn, name)
    bq_table_exists(tb)
  })

#' @rdname DBI
#' @inheritParams DBI::dbListFields
#' @export
setMethod(
  "dbListFields", c("BigQueryConnection", "character"),
  function(conn, name, ...) {
    tb <- as_bq_table(conn, name)
    flds <- bq_table_fields(tb)
    map_chr(flds, function(x) x$name)
  })

#' @rdname DBI
#' @inheritParams DBI::dbRemoveTable
#' @export
setMethod(
  "dbRemoveTable", c("BigQueryConnection", "character"),
  function(conn, name, ...) {
    tb <- as_bq_table(conn, name)
    bq_table_delete(tb)
    invisible(TRUE)
  })

# nocov start
#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "BigQueryConnection",
  function(dbObj, ...) {
    list(
      db.version = NA,
      dbname = paste0(c(dbObj@project, dbObj@dataset), collapse = "."),
      username = NA,
      host = NA,
      port = NA
    )
  })

#' @rdname DBI
#' @inheritParams DBI::dbBegin
#' @export
setMethod(
  "dbBegin", "BigQueryConnection",
  function(conn, ...) {
    testthat::skip("Not yet implemented: dbBegin(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbCommit
#' @export
setMethod(
  "dbCommit", "BigQueryConnection",
  function(conn, ...) {
    testthat::skip("Not yet implemented: dbCommit(Connection)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbRollback
#' @export
setMethod(
  "dbRollback", "BigQueryConnection",
  function(conn, ...) {
    testthat::skip("Not yet implemented: dbRollback(Connection)")
  })
# nocov end


# Convert to bq objects ---------------------------------------------------

#' @export
as_bq_dataset.BigQueryConnection <- function(x) {
  bq_dataset(x@project, x@dataset)
}


#' @export
as_bq_table.BigQueryConnection <- function(x, name, ...) {
  pieces <- strsplit(name, ".", fixed = TRUE)[[1]]

  if (length(pieces) > 3) {
    stop(
      "Table name, '", name, "', must have 1-3 components.",
      call. = FALSE
    )
  }
  if (length(pieces) == 1 && is.null(x@dataset)) {
    stop(
      "Table name, '", name, "', must have 2 or 3 components ",
      "when the connection has no dataset",
      call. = FALSE
    )
  }

  switch(length(pieces),
    bq_table(x@project, x@dataset, pieces[[1]]),
    bq_table(x@project, pieces[[1]], pieces[[2]]),
    bq_table(pieces[[1]], pieces[[2]], pieces[[3]])
  )
}

