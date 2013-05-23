#' @importFrom dplyr sql_variant sql_prefix sql_infix
bigquery_sql <- sql_variant(
  "%||%" = sql_prefix("concat"),
  sd = sql_prefix("stddev"),

  # Casting
  as.logical = sql_prefix("boolean"),
  as.numeric = sql_prefix("float"),
  as.double = sql_prefix("float"),
  as.integer = sql_prefix("integer"),
  as.character = sql_prefix("string"),

  # Date/time
  Sys.date = sql_prefix("current_date"),
  Sys.time = sql_prefix("current_time"),

  # Regular expressions
  grepl = function(match, x) {
    sprintf("REGEXP_MATCH(%s, %s)", escape(x), escape(match))
  },
  gsub = function(match, replace, x) {
    sprintf("REGEXP_REPLACE(%s, %s, %s)", escape(x), escape(match),
      escape(replace))
  },

  # stringr equivalents
  str_detect = function(x, match) {
    sprintf("REGEXP_MATCH(%s, %s)", escape(x), escape(match))
  },
  str_extract = function(x, match) {
    sprintf("REGEXP_EXTRACT(%s, %s)", escape(x), escape(match))
  },
  str_replace = function(x, match, replace) {
    sprintf("REGEXP_REPLACE(%s, %s, %s)", escape(x), escape(match),
      escape(replace))
  }
)

#' @importFrom dplyr partial_eval to_sql_q
trans_bigquery <- function(x, data, env = NULL) {
  if (!is.null(env)) {
    x <- partial_eval(x, data, env)
  }
  if (is.list(x)) {
    vapply(x, to_sql_q, variant = bigquery_sql, FUN.VALUE = character(1))
  } else {
    to_sql_q(x, bigquery_sql)
  }
}
