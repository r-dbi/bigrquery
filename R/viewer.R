


on_connection_opened <- function(connection) {
  observer <- getOption("connectionObserver")
  if (is.null(observer))
    return(invisible(NULL))

  observer$connectionOpened(
    type = "bigquery",
    displayName = "BigQuery",
    host = "https://bigquery.cloud.google.com",
    icon = "https://bigquery.cloud.google.com/resources/favicon.ico",
    connectCode = connection@billing,
    disconnect = function() {
      bigrquery::dbDisconnect(connection)
    },
    listObjectTypes = function() {
      list(
        organisation = list(
          contains = list(
            project = list(
              contains = list(
                dataset = list(
                  contains = list(
                    table = list(contains = "data")
                  )
                )
              )
            )
      )))
    },
    listObjects = function(...) {
      list_bigquery_objects(...)
    },
    listColumns = function(project = NULL, dataset = NULL, table = NULL, ...) {
      x <- bq_table(project, dataset, table)
      fields <- bigrquery::bq_table_fields(x)
      as.data.frame(fields)
    },
    previewObject = function(rowLimit, project = NULL, dataset = NULL, table = NULL, ...) {
      x <- bq_table(project, dataset, table)
      bq_table_download(x, max_results = rowLimit)
    },
    connectionObject = connection
  )
}

#' Lists BigQuery objects for the connection
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
    projects <- bigrquery::list_projects()
    return(data.frame(
      name = projects,
      type = rep("project", length(projects)),
      stringsAsFactors = FALSE
    ))
  }

  if (is.null(dataset)) {
    datasets <- bigrquery::list_datasets(project = project)
    return(
      data.frame(
        name = datasets,
        type = rep("dataset", length(datasets)),
        stringsAsFactors = FALSE
      )
    )
  }

  tables <- bigrquery::list_tables(project = project, dataset = dataset)

  return(
    data.frame(
      name = tables,
      type = rep("table", length(tables)),
      stringsAsFactors = FALSE
    )
  )

}
