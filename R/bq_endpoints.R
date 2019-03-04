#' List BigQuery endpoints
#'
#' Returns a list of BigQuery API v2 endpoints, as stored inside the bigrquery
#' package. The names of this list (or the `id` sub-elements) are the nicknames
#' that can be used to specify an endpoint in [request_generate()]. For each
#' endpoint, we store its nickname or `id`, the associated HTTP verb, the
#' `path`, and details about the parameters. This list is derived
#' programmatically from the [BigQuery API v2 Discovery
#' Document](https://www.googleapis.com/discovery/v1/apis/bigquery/v2/rest).
#' Vocabulary note: we use the term "endpoints", but Google's API docs usually
#' refer to these as "methods".
#'
#' @param i The name(s) or integer index(ices) of the endpoints to return.
#'   Optional. By default, the entire list is returned.
#'
#' @return A list containing some or all of the BigQuery API v2 endpoints that
#'   are used internally by bigrquery.
#' @export
#'
#' @examples
#' names(bq_endpoints())
#' str(bq_endpoints(), max.level = 2)
#' bq_endpoints("bigquery.tables.get")
bq_endpoints <- function(i = NULL) {
  i <- i %||% seq_along(.endpoints)
  ## FIXME: do we care that the attributes are stripped by `[`?
  ## For example, we currently store the base_url and Disc Doc date that way.
  .endpoints[i]
}
