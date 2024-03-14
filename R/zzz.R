.onLoad <- function(libname, pkgname) {
  .auth <<- gargle::init_AuthState(
    package     = "bigrquery",
    auth_active = TRUE
  )

  if (has_internal_auth() && in_pkgdown()) {
    bq_auth_internal()
  }

  # S3 methods --------------------------------------------------------------
  s3_register("dplyr::tbl", "BigQueryConnection")
  s3_register("dplyr::collect", "tbl_BigQueryConnection")
  s3_register("dplyr::same_src", "tbl_BigQueryConnection")

  s3_register("dbplyr::dbplyr_edition", "BigQueryConnection")
  s3_register("dbplyr::db_compute", "BigQueryConnection")
  s3_register("dbplyr::db_copy_to", "BigQueryConnection")
  s3_register("dbplyr::sql_join_suffix", "BigQueryConnection")
  s3_register("dbplyr::sql_translation", "BigQueryConnection")

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
