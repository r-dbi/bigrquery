# nocov start

# Capture connection expression for pane
connection_capture <- function() {
  if (is.null(getOption("connectionObserver"))) {
    return()
  }

  addTaskCallback(function(expr, ...) {
    tryCatch({
      # notify if this is an assignment we can replay
      if (is_call(expr, c("<-", "=")) && is_call(expr[[3]], "dbConnect")) {
        on_connection_opened(
          eval(expr[[2]]),
          paste(c("library(bigrquery)", deparse(expr)), collapse = "\n")
        )
      }
    }, error = function(e) {
      warning("Could not notify connection observer. ", e$message, call. = FALSE)
    })

    # always return false so the task callback is run at most once
    FALSE
  })
}

# https://rstudio.github.io/rstudio-extensions/connections-contract.html#connection-closed
on_connection_closed <- function(con) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionClosed(bq_type, con@project)
}

# https://rstudio.github.io/rstudio-extensions/connections-contract.html#connection-updated
on_connection_updated <- function(con) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionUpdated(bq_type, con@project)
}

# https://rstudio.github.io/rstudio-extensions/connections-contract.html#connection-opened
on_connection_opened <- function(con, code) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionOpened(
    type = bq_type,
    displayName = paste0(c(bq_type, con@project), collapse = "-"),
    host = con@project,
    icon = system.file("icons/bigquery-512-color.png", package = "bigrquery"),

    # connection code
    connectCode = code,

    # only action is to close connections pane
    disconnect = function() dbDisconnect(con),

    listObjectTypes = function() {
      list(
        catalog = list(
          contains = list(
            database = list(
              contains = list(
                table = list(contains = "data"),
                view = list(contains = "data")
              )
            )
          )
        )
      )
    },

    # table enumeration code
    listObjects = function(...) {
      list_bigquery_objects(con, ...)
    },

    # column enumeration code
    listColumns = function(catalog = NULL, database = NULL, table = NULL, view = NULL, ...) {
      x <- bq_table(catalog, database, paste0(table, view))
      fields <- bq_table_fields(x)
      data.frame(
        name = vapply(fields, `[[`, character(1), "name"),
        type = vapply(fields, `[[`, character(1), "type"),
        stringsAsFactors = FALSE
      )
    },

    # table preview code
    previewObject = function(rowLimit, catalog = NULL, database = NULL, table = NULL, view = NULL, ...) {
      x <- bq_table(catalog, database, paste0(table, view))
      bq_table_download(x, max_results = rowLimit)
    },

    # no actions

    # raw connection object
    connectionObject = con

  )
}

list_bigquery_objects <- function(con, catalog = NULL, database = NULL, ...) {
  if (is.null(catalog)) {
    tibble::tibble(type = "catalog", name = con@project)
  } else if (is.null(database)) {
    # Catching VPC/Permission errors to crash gracefully
    bq_datasets <- tryCatch(
      bq_project_datasets(catalog, warn = FALSE),
      error = function(e) list()
    )
    datasets <- map_chr(bq_datasets, `[[`, "dataset")

    tibble::tibble(type = "database", name = datasets)
  } else {
    # Catching VPC/Permission errors to crash gracefully
    ds <- bq_dataset(catalog, database)
    bq_tables <- tryCatch(bq_dataset_tables(ds), error = function(e) list())
    tables <- map_chr(bq_tables, `[[`, "table")
    types <- map_chr(bq_tables, `[[`, "type")
    types <- grepl("VIEW$", types) + 1L
    types <- c("table", "view")[types]
    tibble::tibble(type = types, name = tables)
  }
}

bq_type <- "BigQuery"

# nocov end
