#' @export
#' @title Show RECORD: repeated columns in a BigQuery table
#' @description BigQuery can store data in RECORD: repeated columns in a table. These are structured arrays of data function as nested data frames. RECORD: repeated columns cannot be queried directly and must be unnested using BigQuery's `UNNEST()` function.
#' @param con BigQueryConnection object
#' @param tbl_name Character string of BigQuery dataset.table
#' @examples
#'
#' get_records(con, "eg_dataset.eg_table")

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
