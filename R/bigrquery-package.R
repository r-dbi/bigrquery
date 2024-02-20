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
#' @keywords internal
#' @useDynLib bigrquery, .registration = TRUE
#' @rawNamespace import(rlang, except = unbox)
"_PACKAGE"

the <- new_environment()

# We import rlang this way because jsonlite's unbox() is used extensively.

## usethis namespace: start
#' @importFrom bit64 integer64
#' @importFrom jsonlite unbox
#' @importFrom lifecycle deprecated
#' @importFrom tibble tibble
## usethis namespace: end
NULL
