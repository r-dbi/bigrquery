dots <- function(...) {
  eval(substitute(alist(...)))
}

named_dots <- function(...) {
  args <- dots(...)

  nms <- names2(args)
  missing <- nms == ""
  if (all(!missing)) return(args)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(args[missing], deparse2, character(1), USE.NAMES = FALSE)

  names(args)[missing] <- defaults
  args
}

as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

timer <- function() {
  start <- proc.time()[[3]]
  function() {
    proc.time()[[3]] - start
  }
}

"%||%" <- function(x, y) if(is.null(x)) y else x

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

# Given a string and a separator, split the string at the rightmost
# occurrence of the separator and return the two parts.
rsplit_one <- function(str, sep) {
  assert_that(is.string(str), is.string(sep))
  parts <- strsplit(str, sep, fixed = TRUE)[[1]]
  right <- parts[length(parts)]
  if (length(parts) > 1) {
    left <- paste0(parts[-length(parts)], collapse = sep)
  } else {
    left <- NULL
  }
  list(left = left, right = right)
}

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


size_units <- function(x) {
  i <- floor(log2(x) / 10)
  unit <- c("", "kilo", "mega", "giga", "tera", "peta", "exa", "zetta", "yotta")[i + 1]

  structure(x, i = i, unit = unit, class = "size")
}
#' @export
format.size <- function(x, ...) {
  if (x == 0) return("0 bytes")

  y <- x * 1024 ^ -attr(x, "i")
  sprintf("%.1f %sbytes", y, attr(x, "unit"))
}
