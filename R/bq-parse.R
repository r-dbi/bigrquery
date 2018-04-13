bq_parse_simple <- function(value, type) {
  bq_parse_single(value, bq_field("", type))
}

bq_parse_single <- function(value, field) {
  field <- jsonlite::toJSON(as_json(field))
  value <- jsonlite::toJSON(value, auto_unbox = TRUE)

  bq_field_init(field, value)
}

v <- function(x) list(v = x)
vs <- function(...) lapply(list(...), v)
f <- function(...) list(f = list(...))
