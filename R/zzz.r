.onAttach <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    bigquery.quiet = NA
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])

  invisible()
}
