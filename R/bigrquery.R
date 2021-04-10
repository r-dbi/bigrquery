#' @section Package options:
#' \describe{
#'   \item{`bigrquery.quiet`}{Verbose output during processing? The default
#'   value, `NA`, turns on verbose output for queries that run longer than
#'   two seconds.  Use `FALSE` for immediate verbose output, `TRUE`
#'   for quiet operation.}
#'
#'   \item{`bigrquery.page.size`}{Default page size for fetching data,
#'   defaults to 1e4.}
#' }
#'
#' @import assertthat
#' @importFrom jsonlite unbox
#' @importFrom Rcpp sourceCpp
#' @importFrom tibble tibble
#' @importFrom bit64 integer64
#' @importFrom rlang %||%
#' @importFrom glue glue glue_data glue_collapse
#' @keywords internal
#' @useDynLib bigrquery, .registration = TRUE
"_PACKAGE"
