# Don't import to avoid build-time dependency
sql_prefix <- function(f, n = NULL) {
  dbplyr::sql_prefix(f = f, n = n)
}

# registered onLoad
sql_join_suffix.BigQueryConnection <- function(con, ...) {
  c("_x", "_y")
}

# registered onLoad
sql_translation.BigQueryConnection <- function(x) {
  dbplyr::sql_variant(
    dbplyr::sql_translator(
      .parent = dbplyr::base_scalar,

      `^` = sql_prefix("POW"),
      `%%` = sql_prefix("MOD"),
      "%||%" = sql_prefix("IFNULL"),

      # Coercion
      as.integer = function(x) dbplyr::build_sql("SAFE_CAST(", x, " AS INT64)"),
      as.logical = function(x) {
        dbplyr::build_sql("SAFE_CAST(", x, " AS BOOLEAN)")
      },
      as.numeric = function(x) {
        dbplyr::build_sql("SAFE_CAST(", x, " AS FLOAT64)")
      },
      as.character = function(x) {
        dbplyr::build_sql("SAFE_CAST(", x, " AS STRING)")
      },

      # Date/time
      Sys.date = sql_prefix("current_date"),
      Sys.time = sql_prefix("current_time"),
      floor_date = function(x, unit = "seconds") {
        unit <- bq_date_unit(unit)
        bq_floor_date(x, unit)
      },
      ceiling_date = function(x, unit = "seconds", change_on_boundary = NULL) {
        unit <- bq_date_unit(unit)
        bq_ceiling_date(x, unit, change_on_boundary)
      },
      round_date = function(x, unit = "seconds") {
        unit <- bq_date_unit(unit)
        lo <- bq_floor_date(x, unit)
        hi <- bq_ceiling_date(x, unit, change_on_boundary = FALSE)
        dbplyr::sql_glue("IF(({x}) - {lo} < {hi} - ({x}), {lo}, {hi})")
      },

      # Regular expressions
      grepl = function(pattern, x) {
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/string_functions#regexp_contains
        dbplyr::build_sql("REGEXP_CONTAINS", list(x, pattern))
      },
      gsub = function(pattern, replace, x) {
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/string_functions#regexp_replace
        dbplyr::build_sql("REGEXP_REPLACE", list(x, pattern, replace))
      },

      # Other scalar functions
      ifelse = sql_prefix("IF"),

      # string
      paste0 = sql_prefix("CONCAT"),

      # stringr equivalents
      str_detect = sql_prefix("REGEXP_CONTAINS", 2),
      str_extract = sql_prefix("REGEXP_EXTRACT", 2),
      str_replace = sql_prefix("REGEXP_REPLACE", 3),

      # Parallel min and max
      pmax = sql_prefix("GREATEST"),
      pmin = sql_prefix("LEAST"),

      runif = function(n = n(), min = 0, max = 1) {
        RAND <- NULL # quiet R CMD check
        dbplyr::sql_runif(RAND(), n = {{ n }}, min = min, max = max)
      },
      # clock functions
      add_days = function(x, n, ...) {
        check_dots_empty()
        dbplyr::sql_glue("DATE_ADD(CAST({x} AS DATE), INTERVAL {n} DAY)")
      },
      add_years = function(x, n, ...) {
        check_dots_empty()
        dbplyr::sql_glue("DATE_ADD(CAST({x} AS DATE), INTERVAL {n} YEAR)")
      },
      date_build = function(year, month = 1L, day = 1L, ..., invalid = NULL) {
        check_dots_empty()
        check_unsupported_arg(invalid, allow_null = TRUE)
        dbplyr::sql_glue("DATE({year}, {month}, {day})")
      },
      date_count_between = function(start, end, precision, ..., n = 1L) {
        check_dots_empty()
        check_unsupported_arg(precision, allowed = "DAY")
        check_unsupported_arg(n, allowed = 1L)
        dbplyr::sql_glue(
          "DATE_DIFF(CAST({end} AS DATE), CAST({start} AS DATE), DAY)"
        )
      },
      get_year = function(x) dbplyr::sql_glue("EXTRACT(YEAR FROM {x})"),
      get_month = function(x) dbplyr::sql_glue("EXTRACT(MONTH FROM {x})"),
      get_day = function(x) dbplyr::sql_glue("EXTRACT(DAY FROM {x})"),
      difftime = function(time1, time2, tz, units = "DAY") {
        check_unsupported_arg(tz)
        check_unsupported_arg(units, allowed = "DAY")
        dbplyr::sql_glue(
          "DATE_DIFF(CAST({time1} AS DATE), CAST({time2} AS DATE), DAY)"
        )
      },
    ),
    dbplyr::sql_translator(
      .parent = dbplyr::base_agg,

      n = function() dplyr::sql("count(*)"),

      all = sql_prefix("LOGICAL_AND", 1),
      any = sql_prefix("LOGICAL_OR", 1),

      sd = sql_prefix("STDDEV_SAMP"),
      var = sql_prefix("VAR_SAMP"),
      cor = dbplyr::sql_aggregate_2("CORR"),
      cov = dbplyr::sql_aggregate_2("COVAR_SAMP"),

      # Median
      median = function(x, na.rm = TRUE) {
        dbplyr::build_sql("APPROX_QUANTILES(", x, ", 2)[SAFE_ORDINAL(2)]")
      }
    ),
    dbplyr::sql_translator(
      .parent = dbplyr::base_win,

      all = dbplyr::win_absent("LOGICAL_AND"),
      any = dbplyr::win_absent("LOGICAL_OR"),

      sd = dbplyr::win_recycled("STDDEV_SAMP"),
      var = dbplyr::win_recycled("VAR_SAMP"),
      cor = dbplyr::win_absent("CORR"),
      cov = dbplyr::win_absent("COVAR_SAMP"),

      n_distinct = dbplyr::win_absent("n_distinct"),

      median = dbplyr::win_absent("median")
    )
  )
}

simulate_bigrquery <- function(use_legacy_sql = FALSE) {
  new(
    "BigQueryConnection",
    project = "test",
    dataset = "test",
    billing = "test",
    use_legacy_sql = use_legacy_sql,
    page_size = 0L,
    quiet = TRUE
  )
}

# lubridate rounding helpers. These use DATE_TRUNC() and DATE_ADD() because
# both are overloaded for DATE, DATETIME, and TIMESTAMP, and preserve the type.
bq_date_unit <- function(unit, call = caller_env()) {
  unit <- arg_match0(
    sub("s$", "", unit),
    c("second", "minute", "hour", "day", "week", "month", "quarter", "year"),
    arg_nm = "unit",
    error_call = call
  )
  toupper(unit)
}

bq_floor_date <- function(x, unit) {
  # https://cloud.google.com/bigquery/docs/reference/standard-sql/date_functions#date_trunc
  dbplyr::sql_glue("DATE_TRUNC({x}, {.sql unit})")
}

bq_ceiling_date <- function(x, unit, change_on_boundary = NULL) {
  floor <- bq_floor_date(x, unit)
  # TIMESTAMPs can't add units larger than a day, so for larger units add
  # enough days to reach the next period, then truncate again
  days <- c(WEEK = 7L, MONTH = 31L, QUARTER = 92L, YEAR = 366L)
  # https://cloud.google.com/bigquery/docs/reference/standard-sql/date_functions#date_add
  if (unit %in% names(days)) {
    next_floor <- bq_floor_date(
      dbplyr::sql_glue("DATE_ADD({floor}, INTERVAL {days[[unit]]} DAY)"),
      unit
    )
  } else {
    next_floor <- dbplyr::sql_glue("DATE_ADD({floor}, INTERVAL 1 {.sql unit})")
  }
  # https://lubridate.tidyverse.org/reference/round_date.html#rounding-up-date-objects
  if (is.null(change_on_boundary)) {
    dbplyr::sql_glue(
      "IF(TYPEOF({x}) != 'DATE' AND {floor} = {x}, {x}, {next_floor})"
    )
  } else if (change_on_boundary) {
    next_floor
  } else {
    dbplyr::sql_glue("IF({floor} = {x}, {x}, {next_floor})")
  }
}
