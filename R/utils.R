as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

check_quiet <- function(x, arg = caller_arg(x), call = caller_env(call)) {
  check_bool(x, allow_na = TRUE, arg = arg, call = call)

  if (is.na(x)) {
    !(is_interactive() || is_snapshot())
  } else {
    x
  }
}

bq_check_namespace <- function(pkg, bq_type) {
  check_installed(pkg, sprintf("to parse BigQuery '%s' fields.", bq_type))
}

isFALSE <- function(x) identical(x, FALSE)

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
# nocov end

defer <- function(expr, env = caller_env(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}

in_pkgdown <- function() {
  identical(Sys.getenv("IN_PKGDOWN"), "true")
}

as_query <- function(x, error_arg = caller_arg(x), error_call = caller_env()) {
  if (is(x, "SQL")) {
    x <- x@.Data
  }
  check_string(x, arg = error_arg, call = error_call)
  x
}

has_bigrquerystorage <- function() {
  is_installed("bigrquerystorage")
}

check_labels <- function(labels) {
  # Handle NULL, NA, or empty inputs
  if (is.null(labels) || length(labels) == 0 || (length(labels) == 1 && is.na(labels))) {
    return(NULL)
  }

  if (!is.list(labels)) {
    warning(paste0("Labels must to be a dictionary list; dropping labels"), immediate. = TRUE)
    return(NULL)
  }
  nms <- names(labels)
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    warning("Label keys must be non-empty strings; dropping labels", immediate. = TRUE, call. = FALSE)
    return(NULL)
  }
  for (nm in names(labels)) {
    if (!is.character(labels[[nm]]) || length(labels[[nm]]) != 1) {
      warning(sprintf("Label '%s' must be a single string; dropping labels", nm), immediate. = TRUE)
      return(NULL)
    }
    if (nm != tolower(nm)) {
      warning(sprintf("Label key '%s' must match ^[a-z0-9_-]{0,62}$; dropping labels", nm), immediate. = TRUE)
      return(NULL)
    }
    if (labels[[nm]] != tolower(labels[[nm]])) {
      warning(sprintf("Label value '%s' must be empty or match ^[a-z0-9_-]{0,62}$; dropping labels", labels[[nm]]), immediate. = TRUE)
      return(NULL)
    }
  }

  return(labels)
}
