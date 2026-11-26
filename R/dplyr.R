#' A BigQuery data source for dplyr.
#'
#' Create the connection to the database with `DBI::dbConnect()` then
#' use [dplyr::tbl()] to connect to tables within that database. Generally,
#' it's best to provide the fully qualified name of the table (i.e.
#' `project.dataset.table`) but if you supply a default `dataset` in the
#' connection, you can use just the table name. (This, however, will
#' prevent you from making joins across datasets.)
#'
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to `project`
#' @param max_pages (IGNORED) maximum pages returned by a query
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # To run this example, replace billing with the id of one of your projects
#' # set up for billing
#' con <- DBI::dbConnect(bigquery(), project = bq_test_project())
#'
#' shakespeare <- con %>% tbl(I("publicdata.samples.shakespeare"))
#' shakespeare
#' shakespeare %>%
#'   group_by(word) %>%
#'   summarise(n = sum(word_count, na.rm = TRUE)) %>%
#'   arrange(desc(n))
#' }
src_bigquery <- function(project, dataset, billing = project, max_pages = 10) {
  check_installed("dbplyr")

  con <- DBI::dbConnect(
    bigquery(),
    project = project,
    dataset = dataset,
    billing = billing,
    use_legacy_sql = FALSE
  )

  dbplyr::src_dbi(con)
}

# registered onLoad
tbl.BigQueryConnection <- function(src, from, ...) {
  src <- dbplyr::src_dbi(src, auto_disconnect = FALSE)

  sql <- dbplyr::sql_query_fields(src$con, from)
  dataset <- if (!is.null(src$con@dataset)) as_bq_dataset(src$con)
  schema <- bq_perform_query_schema(
    sql,
    billing = src$con@billing,
    default_dataset = dataset,
    labels = src$con@labels
  )
  vars <- map_chr(schema, "[[", "name")

  if (utils::packageVersion("dbplyr") >= "2.4.0.9000") {
    tbl <- dplyr::tbl(src, from = from, vars = vars)
  } else {
    tbl <- dplyr::tbl(src, from = from, vars = vars, check_from = FALSE)
  }

  # This is ugly, but I don't see a better way of doing this
  if (!inherits(from, "sql")) {
    tb <- as_bq_table(src$con, from)
    tbl$lazy_query$is_view <- tryCatch(
      bq_table_meta(tb, "type")$type == "VIEW",
      bigrquery_notFound = function(e) TRUE
    )
  } else {
    tbl$lazy_query$is_view <- FALSE
  }
  tbl
}

# registered onLoad
dbplyr_edition.BigQueryConnection <- function(con) 2L

# registered onLoad
db_compute.BigQueryConnection <- function(
  con,
  table,
  sql,
  ...,
  overwrite = FALSE,
  temporary = TRUE,
  unique_indexes = list(),
  indexes = list(),
  analyze = TRUE,
  in_transaction = FALSE
) {
  if (is.null(con@dataset)) {
    destination_table <- if (!temporary) as_bq_table(con, table)
    tb <- bq_project_query(
      con@project,
      sql,
      destination_table = destination_table,
      labels = con@labels
    )
  } else {
    ds <- bq_dataset(con@project, con@dataset)
    destination_table <- if (!temporary) as_bq_table(con, table)

    tb <- bq_dataset_query(
      ds,
      query = sql,
      destination_table = destination_table,
      labels = con@labels
    )
  }

  dbplyr::as_table_path(
    I(paste(tb$project, tb$dataset, tb$table, sep = ".")),
    con
  )
}

# registered onLoad
db_copy_to.BigQueryConnection <- function(
  con,
  table,
  values,
  ...,
  overwrite = FALSE,
  types = NULL,
  temporary = TRUE,
  unique_indexes = NULL,
  indexes = NULL,
  analyze = TRUE,
  in_transaction = TRUE
) {
  if (temporary) {
    cli::cli_abort("BigQuery does not support temporary tables")
  }

  tb <- as_bq_table(con, table)
  write <- if (overwrite) "WRITE_TRUNCATE" else "WRITE_EMPTY"
  bq_table_upload(
    tb,
    values,
    fields = types,
    write_disposition = write,
    labels = con@labels
  )

  table
}

# exported onLoad
same_src.tbl_BigQueryConnection <- function(x, y) {
  inherits(y, "tbl_BigQueryConnection")
}
