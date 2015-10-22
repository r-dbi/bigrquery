.onAttach <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    bigquery.quiet = NA
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])

  if(any(grepl('key.json', list.files()))) {
    message('Found key.')
    client_secrets <<- jsonlite::fromJSON(
      revmod::ReadFileText('key.json')
    )
  } else {
    warning('No key.json found. You cannot connect to BigQuery.')
  }

  invisible()
}
