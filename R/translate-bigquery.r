#' @importFrom dplyr sql_variant sql_prefix sql_infix
bigquery_sql <- sql_variant(
  sd = sql_prefix("stdev")
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
