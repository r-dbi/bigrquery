#' @include dbi-driver.r
NULL

BigQueryConnection <- function(project, dataset, billing,
                               page_size = 1e4,
                               quiet = NA,
                               use_legacy_sql = TRUE) {
  ret <- new("BigQueryConnection",
    project = project,
    dataset = dataset,
    billing = billing,
    page_size = as.integer(page_size),
    quiet = quiet,
    use_legacy_sql = use_legacy_sql,
    .envir = new.env(parent = emptyenv())
  )
  ret@.envir$valid <- TRUE
  ret
}

#' @rdname DBI
#' @export
setClass(
  "BigQueryConnection",
  contains = "DBIConnection",
  slots = list(
    project = "character",
    dataset = "character",
    billing = "character",
    use_legacy_sql = "logical",
    page_size = "integer",
    quiet = "logical",
    .envir = "environment"
  )
)

#' @rdname DBI
#' @inheritParams methods::show
#' @export
setMethod(
  "show", "BigQueryConnection",
  function(object) {
    cat("<BigQueryConnection>\n",
        "  Dataset: ", dbGetInfo(object)$dbname,
        sep = "")
  })

#' @rdname DBI
#' @inheritParams DBI::dbIsValid
#' @export
setMethod(
  "dbIsValid", "BigQueryConnection",
  function(dbObj, ...) {
    dbObj@.envir$valid
  })

#' @rdname DBI
#' @inheritParams DBI::dbDisconnect
#' @export
setMethod(
  "dbDisconnect", "BigQueryConnection",
  function(conn, ...) {
    if (!dbIsValid(conn)) {
      warning("Connection already closed.", call. = FALSE)
    }

    unset_result(conn)
    conn@.envir$valid <- FALSE

    invisible(TRUE)
  })

#' @rdname DBI
#' @inheritParams DBI::dbSendQuery
#' @export
setMethod(
  "dbSendQuery", c("BigQueryConnection", "character"),
  function(conn, statement, ...) {
    assert_connection_valid(conn)

    unset_result(conn)
    res <- BigQueryResult(
      connection = conn,
      statement = statement
    )
    set_result(conn, res)
    res
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
      if (overwrite) {
        write_disposition <- "WRITE_TRUNCATE"
      } else {
        write_disposition <- "WRITE_EMPTY"
      }
    }

    data <- DBI::sqlRownamesToColumn(value, row.names = row.names)

    job <- insert_upload_job(
      conn@project, conn@dataset, name, data,
      conn@billing,
      create_disposition = create_disposition,
      write_disposition = write_disposition
    )
    job <- wait_for(job, conn@quiet)
    invisible(TRUE)
  })

#' @rdname DBI
#' @inheritParams DBI::dbReadTable
#' @export
setMethod(
  "dbReadTable", c("BigQueryConnection", "character"),
  function(conn, name, ..., row.names = NA) {
    data <- dbGetQuery(conn, paste0("SELECT * FROM ", dbQuoteIdentifier(conn, name)))
    DBI::sqlColumnToRownames(data, row.names = row.names)
  })

#' @rdname DBI
#' @inheritParams DBI::dbListTables
#' @export
setMethod(
  "dbListTables", "BigQueryConnection",
  function(conn, ...) {
    assert_connection_valid(conn)
    list_tables(conn@project, conn@dataset)
  })

#' @rdname DBI
#' @inheritParams DBI::dbExistsTable
#' @export
setMethod(
  "dbExistsTable", c("BigQueryConnection", "character"),
  function(conn, name, ...) {
    assert_connection_valid(conn)
    exists_table(conn@project, conn@dataset, name)
  })

#' @rdname DBI
#' @inheritParams DBI::dbListFields
#' @export
setMethod(
  "dbListFields", c("BigQueryConnection", "character"),
  function(conn, name, ...) {
    testthat::skip("Not yet implemented: dbListFields(Connection, character)")
  })

#' @rdname DBI
#' @inheritParams DBI::dbRemoveTable
#' @export
setMethod(
  "dbRemoveTable", c("BigQueryConnection", "character"),
  function(conn, name, ...) {
    assert_connection_valid(conn)
    delete_table(conn@project, conn@dataset, name)
    invisible(TRUE)
  })

#' @rdname DBI
#' @inheritParams DBI::dbGetInfo
#' @export
setMethod(
  "dbGetInfo", "BigQueryConnection",
  function(dbObj, ...) {
    assert_connection_valid(dbObj)
    list(
      db.version = NA,
      dbname = format_dataset(dbObj@project, dbObj@dataset),
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

unset_result <- function(conn) {
  if (!is.null(conn@.envir$active_result)) {
    warning("Closing active result set.", call. = FALSE)
    dbClearResult(conn@.envir$active_result)
  }
}

set_result <- function(conn, res) {
  conn@.envir$active_result <- res
}

assert_connection_valid <- function(conn) {
  if (!dbIsValid(conn)) {
    stop("Connection has been already closed.", call. = FALSE)
  }
}
