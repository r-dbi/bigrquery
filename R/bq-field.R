#' BiqQuery field (and fields) class
#'
#' `bq_field()` and `bq_fields()` create; `as_bq_field()` and `as_bq_fields()`
#' coerce from lists.
#'
#' @param name Field name
#' @param type Field type
#' @param mode Field mode
#' @param fields For a field of type "record", a list of sub-fields.
#' @param description Field description
#' @param x A list of `bg_fields`
#' @export
#' @examples
#' bq_field("name", "string")
#'
#' as_bq_fields(list(
#'   list(name = "name", type = "string"),
#'   bq_field("age", "integer")
#' ))
#'
#' # as_bq_fields() can also take a data frame
#' as_bq_fields(mtcars)
bq_field <- function(name, type, mode = "NULLABLE", fields = list(), description = NULL) {
  assert_that(is.string(name), is.string(type), is.string(mode))

  structure(
    list(
      name = name,
      type = toupper(type),
      mode = toupper(mode),
      fields = as_bq_fields(fields),
      description = description %||% ""
    ),
    class = "bq_field"
  )
}

#' @export
as_json.bq_field <- function(x) {
  list(
    name = unbox(x$name),
    type = unbox(x$type),
    mode = unbox(x$mode),
    fields = as_json(x$fields),
    description = unbox(x$description)
  )
}

#' @export
#' @rdname bq_field
bq_fields <- function(x) {
  structure(x, class = "bq_fields")
}

#' @export
as_json.bq_fields <- function(x) {
  lapply(x, as_json)
}

#' @export
#' @rdname bq_field
as_bq_field <- function(x) UseMethod("as_bq_field")

#' @export
as_bq_field.bq_field <- function(x) x

#' @export
as_bq_field.list <- function(x) {
  bq_field(
    name = x$name,
    type = x$type,
    mode = x$mode %||% "NULLABLE",
    fields = lapply(x$fields, as_bq_field),
    description = x$description %||% ""
  )
}

#' @export
#' @rdname bq_field
as_bq_fields <- function(x) UseMethod("as_bq_fields")

#' @export
as_bq_fields.bq_fields <- function(x) x

#' @export
as_bq_fields.data.frame <- function(x) {
  types <- vapply(x, data_type, character(1))
  fields <- Map(function(type, name) bq_field(name, type), types, names(x))
  bq_fields(unname(fields))
}

#' @export
as_bq_fields.list <- function(x) {
  bq_fields(lapply(x, as_bq_field))
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
  invisible(x)
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
  invisible(x)
}

data_type <- function(x) {
  if (is.factor(x)) return("STRING")
  if (inherits(x, "POSIXt")) return("TIMESTAMP")
  if (inherits(x, "hms")) return("TIME")
  if (inherits(x, "wk_wkt")) return("GEOGRAPHY")
  if (inherits(x, "blob")) return("BYTES")
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
