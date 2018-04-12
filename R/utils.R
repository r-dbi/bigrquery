as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

"%||%" <- function(x, y) if (is.null(x)) y else x

bq_progress <- function(..., quiet = NA) {
  delay <- if (isFALSE(quiet)) 0 else 1
  quiet <- if (is.na(quiet)) !interactive() else FALSE

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

#' @export
print.bq_bytes <- function(x, ...) {
  cat_line(prettyunits::pretty_bytes(x))
}


as_json <- function(x) UseMethod("as_json")

#' @export
as_json.NULL <- function(x) NULL
