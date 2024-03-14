#' @include dbi-driver.R
NULL

BigQueryConnection <- function(project,
                               dataset,
                               billing,
                               page_size = 1e4,
                               quiet = NA,
                               use_legacy_sql = FALSE,
                               bigint = c("integer", "integer64", "numeric", "character")) {

  connection_capture()

  new("BigQueryConnection",
    project = project,
    dataset = dataset,
    billing = billing,
    page_size = as.integer(page_size),
    quiet = quiet,
    use_legacy_sql = use_legacy_sql,
    bigint = match.arg(bigint)
  )
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
    on_connection_closed(conn)
    invisible(TRUE)
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
#' @export
setMethod(
  "dbSendQuery",
  c("BigQueryConnection", "character"),
  function(conn, statement, ..., params = NULL) {
    BigQueryResult(conn, statement, params = params, ...)
  }
)

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
#' @export
setMethod("dbExecute", c("BigQueryConnection", "character"), function(conn, statement, ...) {
  ds <- if (!is.null(conn@dataset)) as_bq_dataset(conn)

  job <- bq_perform_query(statement,
    billing = conn@billing,
    default_dataset = ds,
    quiet = conn@quiet,
    ...
  )
  bq_job_wait(job, quiet = conn@quiet)

  meta <- bq_job_meta(job, "statistics(query(numDmlAffectedRows))")
  as.numeric(meta$statistics$query$numDmlAffectedRows %||% 0)
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
      cli::cli_abort("{.arg x} must not contain missing values.")
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
#' @export
setMethod(
  "dbQuoteLiteral", c("BigQueryConnection", "logical"),
  function(conn, x, ...) {
    x <- as.character(x)
    x[is.na(x)] <- "NULL"
    SQL(x, names = names(x))
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

dbWriteTable_bq <- function(conn,
                            name,
                            value,
                            ...,
                            overwrite = FALSE,
                            append = FALSE,
                            field.types = NULL,
                            temporary = FALSE,
                            row.names = NA) {

  check_bool(overwrite)
  check_bool(append)

  if (!is.null(field.types)) {
    cli::cli_abort(
      "{.arg field.types} not supported by bigrquery.",
      call = quote(DBI::dbWriteTable())
    )
  }
  if (!identical(temporary, FALSE)) {
    cli::cli_abort(
      "{.code temporary = FALSE} not supported by bigrquery.",
      call = quote(DBI::dbWriteTable())
    )
  }

  if (append) {
    create_disposition <- "CREATE_NEVER"
    write_disposition <- "WRITE_APPEND"
  } else {
    create_disposition <- "CREATE_IF_NEEDED"
    write_disposition <- if (overwrite) "WRITE_TRUNCATE" else "WRITE_EMPTY"
  }
  tb <- as_bq_table(conn, name)

  bq_table_upload(
    tb,
    value,
    create_disposition = create_disposition,
    write_disposition = write_disposition,
    billing = conn@billing,
    ...
  )
  invisible(TRUE)
}

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
  "dbWriteTable",
  c("BigQueryConnection", "character", "data.frame"),
  dbWriteTable_bq
)

#' @rdname DBI
#' @export
setMethod(
  "dbWriteTable",
  c("BigQueryConnection", "Id", "data.frame"),
  dbWriteTable_bq
)

#' @rdname DBI
#' @export
setMethod(
  "dbWriteTable",
  c("BigQueryConnection", "AsIs", "data.frame"),
  dbWriteTable_bq
)


dbAppendTable_bq <- function(conn, name, value, ..., row.names = NULL) {
  tb <- as_bq_table(conn, name)

  bq_table_upload(tb, value,
    create_disposition = "CREATE_NEVER",
    write_disposition = "WRITE_APPEND",
    ...
  )
  on_connection_updated(conn, toString(tb))

  invisible(TRUE)
}

#' @inheritParams DBI::dbAppendTable
#' @rdname DBI
#' @export
setMethod("dbAppendTable", c("BigQueryConnection", "character", "data.frame"), dbAppendTable_bq)

#' @rdname DBI
#' @export
setMethod("dbAppendTable", c("BigQueryConnection", "Id", "data.frame"), dbAppendTable_bq)

#' @rdname DBI
#' @export
setMethod("dbAppendTable", c("BigQueryConnection", "AsIs", "data.frame"), dbAppendTable_bq)

dbCreateTable_bq <- function(conn,
                             name,
                             fields,
                             ...,
                             row.names = NULL,
                             temporary = FALSE) {
  if (!identical(temporary, FALSE)) {
    cli::cli_abort(
      "{.code temporary = FALSE} not supported by bigrquery.",
      call = quote(DBI::dbCreateTable())
    )
  }

  tb <- as_bq_table(conn, name)
  bq_table_create(tb, fields)
  on_connection_updated(conn, toString(tb))

  invisible(TRUE)
}

#' @inheritParams DBI::dbCreateTable
#' @rdname DBI
#' @export
setMethod("dbCreateTable", "BigQueryConnection", dbCreateTable_bq)

#' @rdname DBI
#' @export
setMethod("dbCreateTable", "BigQueryConnection", dbCreateTable_bq)

dbReadTable_bq <- function(conn, name, ...) {
  tb <- as_bq_table(conn, name)
  bq_table_download(tb, ...)
}

#' @rdname DBI
#' @inheritParams DBI::dbReadTable
#' @export
setMethod("dbReadTable", c("BigQueryConnection", "character"), dbReadTable_bq)

#' @rdname DBI
#' @export
setMethod("dbReadTable", c("BigQueryConnection", "Id"), dbReadTable_bq)

#' @rdname DBI
#' @export
setMethod("dbReadTable", c("BigQueryConnection", "AsIs"), dbReadTable_bq)

#' @rdname DBI
#' @inheritParams DBI::dbListTables
#' @export
setMethod(
  "dbListTables", "BigQueryConnection",
  function(conn, ...) {
    if (is.null(conn@dataset)) {
      cli::cli_abort("Can't list tables without a connection `dataset`.")
    }
    ds <- bq_dataset(conn@project, conn@dataset)

    tbs <- bq_dataset_tables(ds, ...)
    map_chr(tbs, function(x) x$table)
  })

dbExistsTable_bq <- function(conn, name, ...) {
  tb <- as_bq_table(conn, name)
  bq_table_exists(tb)
}
#' @inheritParams DBI::dbExistsTable
#' @rdname DBI
#' @export
setMethod("dbExistsTable", c("BigQueryConnection", "character"), dbExistsTable_bq)

#' @rdname DBI
#' @export
setMethod("dbExistsTable", c("BigQueryConnection", "Id"), dbExistsTable_bq)

#' @rdname DBI
#' @export
setMethod("dbExistsTable", c("BigQueryConnection", "AsIs"), dbExistsTable_bq)

dbListFields_bq <- function(conn, name, ...) {
  tb <- as_bq_table(conn, name)
  flds <- bq_table_fields(tb)
  map_chr(flds, function(x) x$name)
}

#' @inheritParams DBI::dbListFields
#' @rdname DBI
#' @export
setMethod("dbListFields", c("BigQueryConnection", "character"), dbListFields_bq)

#' @rdname DBI
#' @export
setMethod("dbListFields", c("BigQueryConnection", "Id"), dbListFields_bq)

#' @rdname DBI
#' @export
setMethod("dbListFields", c("BigQueryConnection", "AsIs"), dbListFields_bq)

dbRemoveTable_bq <- function(conn, name, ...) {
  tb <- as_bq_table(conn, name)
  bq_table_delete(tb)
  on_connection_updated(conn, toString(tb))
  invisible(TRUE)
}

#' @inheritParams DBI::dbRemoveTable
#' @rdname DBI
#' @export
setMethod("dbRemoveTable", c("BigQueryConnection", "character"), dbRemoveTable_bq)

#' @rdname DBI
#' @export
setMethod("dbRemoveTable", c("BigQueryConnection", "Id"), dbRemoveTable_bq)

#' @rdname DBI
#' @export
setMethod("dbRemoveTable", c("BigQueryConnection", "AsIs"), dbRemoveTable_bq)

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
as_bq_dataset.BigQueryConnection <- function(x, ..., error_arg, error_call) {
  bq_dataset(x@project, x@dataset)
}


#' @export
as_bq_table.BigQueryConnection <- function(x, name, ...) {
  if (inherits(name, "dbplyr_table_path")) { # dbplyr 2.5.0
    pieces <- utils::getFromNamespace("table_path_components", "dbplyr")(name, x)[[1]]
  } else if (inherits(name, "dbplyr_table_ident")) { # dbplyr 2.4.0
    name <- unclass(name)
    pieces <- c(name$catalog, name$schema, name$table)
    pieces <- pieces[!is.na(pieces)]

    if (length(pieces) == 1) {
      pieces <- strsplit(pieces, ".", fixed = TRUE)[[1]]
    }
  } else if (is(name, "Id")) {
    pieces <- unname(name@name)
  } else if (is.character(name) && length(name) == 1) {
    # Technically incorrect according to the DBI spec, because it should
    # automatically quote the name; but too high risk to change now
    pieces <- strsplit(name, ".", fixed = TRUE)[[1]]
  } else {
    cli::cli_abort("{.arg name} must be a string or a dbplyr_table_ident.")
  }

  if (length(pieces) > 3) {
    cli::cli_abort("{.arg name} ({.str {name}}) must have 1-3 components.")
  }
  if (length(pieces) == 1 && is.null(x@dataset)) {
    cli::cli_abort(
      "{.arg name} ({.str {name}}) must have 2 or 3 components if the connection doesn't have a dataset."
    )
  }

  switch(length(pieces),
    bq_table(x@project, x@dataset, pieces[[1]]),
    bq_table(x@project, pieces[[1]], pieces[[2]]),
    bq_table(pieces[[1]], pieces[[2]], pieces[[3]])
  )
}
