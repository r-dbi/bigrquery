as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

bq_quiet <- function(x) {
  if (is.na(x)) {
    !interactive()
  } else {
    x
  }
}

bq_progress <- function(..., quiet = NA) {
  # quiet = FALSE -> show immediately; otherwise wait 1 second
  delay <- if (isFALSE(quiet)) 0 else 1
  quiet <- bq_quiet(quiet)

  if (quiet) {
    list(
      tick = function(...) {},
      update = function(...) {}
    )
  } else {
    progress <- progress::progress_bar$new(..., show_after = delay)
    progress$tick(0)
    progress
  }
}

bq_check_namespace <- function(pkg, bq_type) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return()
  }

  rlang::abort(glue::glue(
    "Package '{pkg}' must be installed to load BigQuery field with type '{bq_type}'"
  ))
}

isFALSE <- function(x) identical(x, FALSE)

is_string <- function(x) length(x) == 1L && is.character(x)

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  format(x, big.mark = mark, scientific = FALSE, ...)
}

map_chr <- function(x, f, ...) {
  vapply(x, f, ..., FUN.VALUE = character(1))
}

indent <- function(x, n = 2) {
  space <- paste(rep(" ", n), collapse = "")
  paste0(space, gsub("\n", paste0("\n", space), x, fixed = TRUE))
}

as_json <- function(x) UseMethod("as_json")

#' @export
as_json.NULL <- function(x) NULL

# nocov start
show_json <- function(x) {
  jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)
}

#' @export
print.bq_bytes <- function(x, ...) {
  cat_line(prettyunits::pretty_bytes(unclass(x)))
}

# BigQuery storage --------------------------------------------------------
bqs_initiate <- function(initiated = getOption("bigrquery.bqs_initiated", FALSE)) {
  if (!initiated) {
    if (!isTRUE(Sys.getenv("GRPC_DEFAULT_SSL_ROOTS_FILE_PATH", TRUE))) {
      if (file.exists(grpc_mingw_root_pem_path_detect())) {
        Sys.setenv(GRPC_DEFAULT_SSL_ROOTS_FILE_PATH = grpc_mingw_root_pem_path_detect())
      }
    }
    bqs_init_logger()
    # Issue with parallel arrow as.data.frame on Windows
    if (.Platform$OS.type == "windows") {
      options("arrow.use_threads" = FALSE)
    }
    options("bigrquery.bqs_initiated" = TRUE)
  }
}

# nocov end
