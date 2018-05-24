bq_parse_single <- function(value, type, ...) {
  field <- bq_field("", type, ...)
  field_j <- jsonlite::toJSON(as_json(field))
  value_j <- jsonlite::toJSON(value, auto_unbox = TRUE)

  bq_field_init(field_j, value_j)
}

v <- function(x) list(v = x)
vs <- function(...) lapply(list(...), v)
f <- function(...) list(f = list(...))
