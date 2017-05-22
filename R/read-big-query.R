#' Read a big query from file
#'
#' This function reads a big query from a file, offering basic interpolation.
#'
#' @param query Path to a file which contains a query.
#' @param ... Use to substitute values for parameters in the query.
#' @export
#' @examples
#' \dontrun{
#' # select.bigquery.sql file
#' SELECT fullVisitorId FROM ga_sesssions_@@YDAY LIMIT 100
#'
#' # R code
#' yday <- gsub("-", "", Sys.Date() - 2)
#' query_exec(read_big_query("select.sql", YDAY = yday), ...)
#'
#' }
read_big_query <- function(query, ...) {
  if (!is.string(query) || !file.exists(query)) {
    stop("query argument does not specify a valid file path.", call. = FALSE)
  }

  out <- readChar(query, file.info(query)$size)
  insert_parameters(out, ...)
}

insert_parameters <- function(query, ...) {
  if (!length(list(...))) {
    return(query)
  }

  values <- lapply(c(...), as.character)
  key_values <- Map(c, paste('@', names(values), sep = ""), values)

  for (key_value in key_values) {
    query <- gsub(key_value[1], key_value[2], query)
  }

  query
}
