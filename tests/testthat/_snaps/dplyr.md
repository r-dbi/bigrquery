# can copy_to

    Code
      dplyr::copy_to(con, mtcars)
    Condition
      Error in `db_copy_to()`:
      ! BigQuery does not support temporary tables

# string functions correctly

    Code
      dbplyr::translate_sql(grepl("a.c", x), con = con)
    Output
      <SQL> REGEXP_CONTAINS(`x`, 'a.c')
    Code
      dbplyr::translate_sql(gsub("a.c", "", x), con = con)
    Output
      <SQL> REGEXP_REPLACE(`x`, 'a.c', '')

# lubridate rounding functions translate to correct sql

    Code
      dbplyr::translate_sql(ceiling_date(x, "hour"), con = con)
    Output
      <SQL> IF(DATE_TRUNC(`x`, HOUR) = `x`, `x`, DATE_ADD(DATE_TRUNC(`x`, HOUR), INTERVAL 1 HOUR))
    Code
      dbplyr::translate_sql(ceiling_date(x, "month"), con = con)
    Output
      <SQL> IF(DATE_TRUNC(`x`, MONTH) = `x`, `x`, DATE_TRUNC(DATE_ADD(DATE_TRUNC(`x`, MONTH), INTERVAL 31 DAY), MONTH))
    Code
      dbplyr::translate_sql(round_date(x, "day"), con = con)
    Output
      <SQL> IF(`x` - DATE_TRUNC(`x`, DAY) < IF(DATE_TRUNC(`x`, DAY) = `x`, `x`, DATE_ADD(DATE_TRUNC(`x`, DAY), INTERVAL 1 DAY)) - `x`, DATE_TRUNC(`x`, DAY), IF(DATE_TRUNC(`x`, DAY) = `x`, `x`, DATE_ADD(DATE_TRUNC(`x`, DAY), INTERVAL 1 DAY)))

---

    Code
      dbplyr::translate_sql(floor_date(x, "fortnight"), con = con)
    Condition
      Error in `floor_date()`:
      ! `unit` must be one of "second", "minute", "hour", "day", "week", "month", "quarter", or "year", not "fortnight".

