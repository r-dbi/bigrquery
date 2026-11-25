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

# slice extrema use whole-row aggregates for grouped n = 1

    Code
      dplyr::show_query(dplyr::slice_max(lf, x, n = 1, with_ties = FALSE, by = g))
    Output
      <SQL>
      SELECT bgr_r.*
      FROM (
      SELECT MAX_BY(bgr_t, bgr_t.`x`) AS bgr_r
      FROM (
      SELECT *
      FROM `df`
      WHERE (NOT((`x` IS NULL)))
      ) AS `bgr_t`
      GROUP BY `g`
      ) AS `q01`
    Code
      dplyr::show_query(dplyr::slice_min(dplyr::group_by(lf, g), x, with_ties = FALSE))
    Output
      <SQL>
      SELECT bgr_r.*
      FROM (
      SELECT MIN_BY(bgr_t, bgr_t.`x`) AS bgr_r
      FROM (
      SELECT *
      FROM `df`
      WHERE (NOT((`x` IS NULL)))
      ) AS `bgr_t`
      GROUP BY `g`
      ) AS `q01`

# slice extrema retain dbplyr argument validation

    Code
      dplyr::slice_max(grouped, x, by = g, with_ties = FALSE)
    Condition
      Error in `dplyr::slice_max()`:
      ! Can't supply `by` when `data` is a grouped data frame.

---

    Code
      dplyr::slice_min(grouped, x, by = character(), with_ties = FALSE)
    Condition
      Error in `dplyr::slice_min()`:
      ! Can't supply `by` when `data` is a grouped data frame.

---

    Code
      dplyr::slice_max(lf, x, n = 1, prop = 0.5, by = g, with_ties = FALSE)
    Condition
      Error in `dplyr::slice_max()`:
      ! Must supply exactly one of `n` and `prop` arguments.
