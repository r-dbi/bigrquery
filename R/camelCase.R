camelCase <- function(x) {
  gsub("_(.)", "\\U\\1", x, perl = TRUE)
}

toCamel <- function(x) {
  if (is.list(x)) {
    x[] <- lapply(x, toCamel)
  }

  if (!is.null(names(x))) {
    names(x) <- camelCase(names(x))
  }

  x
}
