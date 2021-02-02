# nocov start

on_connection_closed <- function(connection) {
  # make sure we have an observer
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  type <- class(connection)[1]
  host <- bq_host()
  observer$connectionClosed(type, host)
}

on_connection_updated <- function(connection, hint) {
  # make sure we have an observer
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  type <- class(connection)[1]
  host <- bq_host()
  observer$connectionUpdated(type, host, hint = hint)
}

on_connection_opened <- function(connection, code) {

  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionOpened(
    # connection type
    type = class(connection)[1],

    # name displayed in connection pane
    displayName = bq_display_name(),

    # host key
    host = bq_host(),

    # icon for connection
    icon = bq_connection_icon(),

    # connection code
    connectCode = code,

    # disconnection code
    disconnect = function() {
      dbDisconnect(connection)
    },

    listObjectTypes = function() { list(
      organisation = list(contains = list(
      project = list(contains = list(
      dataset = list(contains = list(
      table = list(contains = "data")
    )))))))
    },

    # table enumeration code
    listObjects = function(...) {
      list_bigquery_objects(...)
    },

    # column enumeration code
    listColumns = function(project = NULL, dataset = NULL, table = NULL, ...) {
      x <- bq_table(project, dataset, table)
      fields <- bq_table_fields(x)
      data.frame(
        name = vapply(fields, `[[`, character(1), "name"),
        type = vapply(fields, `[[`, character(1), "type"),
        stringsAsFactors = FALSE
      )
    },

    # table preview code
    previewObject = function(rowLimit, project = NULL, dataset = NULL, table = NULL, ...) {
      x <- bq_table(project, dataset, table)
      bq_table_download(x, max_results = rowLimit)
    },

    # no actions

    # raw connection object
    connectionObject = connection

  )
}

#' Lists BigQuery objects for the connection
#' @noRd
list_bigquery_objects <- function(organisation = NULL, project = NULL, dataset = NULL) {
  if (is.null(organisation)) {
    return(
      data.frame(
        name = "Organisation",
        type = "organisation",
        stringsAsFactors = FALSE
      )
    )
  }

  if (is.null(project)) {
    projects <- bq_projects()
    return(data.frame(
      name = projects,
      type = rep("project", length(projects)),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(dataset)) {
    # Catching VPC/Permission errors to crash gracefully
    datasets <- tryCatch(
      {
        bq_project_datasets(project)
      },
      error = function(e) {
        list()
      }
    )
    return(
      data.frame(
        name = vapply(datasets, `[[`, character(1), "dataset"),
        type = rep("dataset", length(datasets)),
        stringsAsFactors = FALSE
      )
    )
  }

  # Catching VPC/Permission errors to crash gracefully
  tables <- tryCatch(
    {
      bq_dataset_tables(
        bq_dataset(project = project, dataset = dataset)
      )
    },
    error = function(e) {
      list()
    }
  )
  return(
    data.frame(
      name = vapply(tables, `[[`, character(1), "table"),
      # TODO: Maybe modify bq_dataset_tables to return type?
      # Type is used to select which icon to display in the pane
      type = rep("table", length(tables)),
      stringsAsFactors = FALSE
    )
  )

}

#' BigQuery host
#' @noRd
bq_host <- function() {
  return("https://bigquery.cloud.google.com")
}

#' BigQuery connection display name
#' @noRd
bq_display_name <- function() {
  return("BigQuery")
}

#' BigQuery connection icon
#' @noRd
bq_connection_icon <- function() {
  system.file("icons/bigquery-512-color.png", package = "bigrquery")
}

# nocov end
