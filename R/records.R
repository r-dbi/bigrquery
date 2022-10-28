#' @export
#' @title Show RECORD: repeated columns in a BigQuery table
#' @param con BigQueryConnection object
#' @param tbl_name Character string of BigQuery dataset.table
#' @examples
#' # NOT RUN
#' # start_con
#'
#' has_records(con, "eg_table.eg_sample")

get_records <- function(con, tbl_name) {
  bq_tab <- bigrquery::as_bq_table(con, tbl_name)

  fields <- bigrquery::bq_table_fields(bq_tab)

  records <- purrr::keep(fields, ~.x$type == "RECORD")

  record_cols <- purrr::map(records, ~{
    record_name <- .x$name
    record_fields <- purrr::map_chr(.x$fields, ~.x$name)
    record_fields
  }) %>%
    purrr::set_names(purrr::map_chr(records, ~.x$name))

  if(length(record_cols) == 0) {
    return(NULL)
  } else {
    return(record_cols)
  }

}
