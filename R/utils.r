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
