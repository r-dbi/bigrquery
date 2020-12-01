.onLoad <- function(libname, pkgname) {

  .auth <<- gargle::init_AuthState(
    package     = "bigrquery",
    auth_active = TRUE
  )

  # S3 methods --------------------------------------------------------------
  register_s3_method("dplyr", "collect", "tbl_BigQueryConnection")
  register_s3_method("dplyr", "db_analyze", "BigQueryConnection")
  register_s3_method("dplyr", "db_query_fields", "BigQueryConnection")
  register_s3_method("dplyr", "db_save_query", "BigQueryConnection")
  register_s3_method("dplyr", "sql_translate_env", "BigQueryConnection")
  register_s3_method("dbplyr", "db_copy_to", "BigQueryConnection")

  if (rlang::is_installed("dbplyr") && utils::packageVersion("dbplyr") > "1.99") {
    register_s3_method("dbplyr", "sql_join_suffix", "BigQueryConnection")
  }

  # Default options --------------------------------------------------------
  op <- options()
  defaults <- list(
    bigrquery.quiet = NA,
    bigrquery.page.size = 1e4
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])

  # BigQuery storage --------------------------------------------------------

  if (!isTRUE(Sys.getenv("GRPC_DEFAULT_SSL_ROOTS_FILE_PATH", TRUE))) {
    if (file.exists(grpc_mingw_root_pem_path_detect)) {
      Sys.setenv(GRPC_DEFAULT_SSL_ROOTS_FILE_PATH = grpc_mingw_root_pem_path_detect)
    }
  }
  bqs_init_logger()
  # Issue with parallel arrow as.data.frame on Windows
  if (Sys.info()[["sysname"]] == "Windows") {
    options("arrow.use_threads" = FALSE)
  }

  invisible()
}

PACKAGE_NAME <- utils::packageName()
PACKAGE_VERSION <- utils::packageVersion(PACKAGE_NAME)

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

.onUnload <- function(libpath) {
  library.dynam.unload("bigrquery", libpath)
}
