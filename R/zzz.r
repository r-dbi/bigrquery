.onLoad <- function(libname, pkgname) {

  # S3 methods --------------------------------------------------------------
  register_s3_method("dplyr", "db_query_fields", "BigQueryConnection")
  register_s3_method("dplyr", "sql_translate_env", "BigQueryConnection")
  register_s3_method("dplyr", "tbl", "src_bigquery")

  # Default options --------------------------------------------------------
  op <- options()
  defaults <- list(
    bigrquery.quiet = NA,
    bigrquery.page.size = 1e4
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])

  invisible()
}

PACKAGE_NAME <- utils::packageName()
PACKAGE_VERSION <- utils::packageVersion(PACKAGE_NAME)
