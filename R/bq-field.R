#' BiqQuery field (and fields) class
#'
#' @param name Field name
#' @param type Field type
#' @param mode Field mode
#' @param fields For a field of type "record", a list of sub-fields.
#' @param x A list of `bg_fields`
#' @export
bq_field <- function(name, type, mode = "nullable", fields = NULL) {
  structure(
    list(
      name = name,
      type = type,
      mode = mode,
      fields = bq_fields(fields)
    ),
    class = "bq_field"
  )
}

#' @export
#' @rdname bq_field
bq_fields <- function(x) {
  structure(x, class = "bq_fields")
}


as_bq_field <- function(x) {
  bq_field(
    name = x$name,
    type = x$type,
    mode = x$mode,
    fields = lapply(x$fields, as_bq_field)
  )
}


#' @export
format.bq_fields <- function(x, ...) {
  if (length(x) == 0) {
    return("")
  }

  fields <- lapply(x, format)
  gsub("\\n\\s+$", "\n", indent(paste0(fields, collapse = "")))
}

#' @export
print.bq_fields <- function(x, ...) {
  cat_line("<bq_fields>\n", format(x, ...))
}

#' @export
format.bq_field <- function(x, ...) {
  type <- x$type
  if (x$mode != "NULLABLE") {
    type <- paste0(type, ": ", tolower(x$mode))
  }

  paste0(x$name, " <", type, ">", "\n", format(x$fields))
}

#' @export
print.bq_field <- function(x, ...) {
  cat_line("<bq_field> ", format(x, ...))
}

as_bq_fields <- function(data) {
  types <- vapply(data, data_type, character(1))
  unname(Map(function(type, name) list(name = name, type = type), types, names(data)))
}

data_type <- function(x) {
  if (is.factor(x)) return("STRING")
  if (inherits(x, "POSIXt")) return("TIMESTAMP")
  if (inherits(x, "hms")) return("TIME")
  if (inherits(x, "Date")) return("DATE")

  switch(
    typeof(x),
    character = "STRING",
    logical = "BOOLEAN",
    double = "FLOAT",
    integer = "INTEGER",
    stop("Unsupported type: ", typeof(x), call. = FALSE)
  )
}
