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
on_connection_updated <- function(con, hint) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionUpdated(bq_type, con@project, hint = hint)
}

# https://rstudio.github.io/rstudio-extensions/connections-contract.html#connection-opened
on_connection_opened <- function(con, code) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionOpened(
    type = bq_type,
    displayName = paste0(bq_type, ": ", con@project),
    host = con@project,
    icon = system.file("icons/bigquery-512-color.png", package = "bigrquery"),

    # connection code
    connectCode = code,

    # only action is to close connections pane
    disconnect = function() {},

    listObjectTypes = function() {
      list(
        dataset = list(
          icon = system.file("icons/dataset.png", package = "bigrquery"),
          contains = list(
            table = list(
              icon = system.file("icons/table.png", package = "bigrquery"),
              contains = "data"
            ),
            view = list(
              icon = system.file("icons/view.png", package = "bigrquery"),
              contains = "data"
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
    listColumns = function(dataset = con@dataset, table = NULL, view = NULL, ...) {
      x <- bq_table(con@project, dataset, paste0(table, view))
      fields <- bq_table_fields(x)

      tibble::tibble(
        name = vapply(fields, `[[`, character(1), "name"),
        type = vapply(fields, `[[`, character(1), "type")
      )
    },

    # table preview code
    previewObject = function(rowLimit, dataset = con@dataset, table = NULL, view = NULL, ...) {
      x <- bq_table(con@project, dataset, paste0(table, view))
      bq_table_download(x, max_results = rowLimit)
    },

    # no actions

    # raw connection object
    connectionObject = con

  )
}

list_bigquery_objects <- function(con, dataset = con@dataset, ...) {
  if (is.null(dataset)) {
    # Catching VPC/Permission errors to crash gracefully
    bq_datasets <- tryCatch(
      bq_project_datasets(con@project, warn = FALSE),
      error = function(e) list()
    )
    datasets <- map_chr(bq_datasets, `[[`, "dataset")

    tibble::tibble(type = "dataset", name = datasets)
  } else {
    # Catching VPC/Permission errors to crash gracefully
    ds <- bq_dataset(con@project, dataset)
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
