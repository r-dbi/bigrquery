.onAttach <- function(libname, pkgname) {
  op <- options()
  defautlts <- list(
    bigquery.quiet = NA
  )
  toset <- !(names(defautlts) %in% names(op))
  if (any(toset)) options(defautlts[toset])
  
  invisible()
}
