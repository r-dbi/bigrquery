bq_parse_single <- function(value, type, ...) {
  field <- bq_field("", type, ...)
  field_j <- jsonlite::toJSON(as_json(field))

  jsonargs <- getOption("bigrquery.jsonlite.toJSON")
  if (!"digits" %in% names(jsonargs)) {
    dig <- getOption("bigrquery.digits")
    jsonargs$digits <- check_digits(dig)
  }
  value_j <- do.call(
    jsonlite::toJSON,
    c(list(value, auto_unbox = TRUE),
      jsonargs[!names(jsonargs) %in% "auto_unbox"])
  )

  bq_field_init(field_j, value_j)
}

v <- function(x) list(v = x)
vs <- function(...) lapply(list(...), v)
f <- function(...) list(f = list(...))
