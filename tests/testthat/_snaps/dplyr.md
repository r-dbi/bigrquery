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

# slice_*() use ARRAY_AGG() when with_ties = FALSE

    Code
      dplyr::show_query(dplyr::slice_max(lf, x, n = 2, with_ties = FALSE, by = g))
    Output
      <SQL>
      SELECT `bigrquery_slice`.`g`, `bigrquery_slice`.`x`, `bigrquery_slice`.`y`
      FROM (
        SELECT ARRAY_AGG(`bigrquery_row` ORDER BY `bigrquery_row`.`bigrquery_order` DESC LIMIT 2) AS `bigrquery_rows`
        FROM (
      SELECT *, `x` AS `bigrquery_order`
      FROM `df`
      WHERE (NOT((`x` IS NULL)))
        ) AS `bigrquery_row`
        GROUP BY `bigrquery_row`.`g`
      ), UNNEST(`bigrquery_rows`) AS `bigrquery_slice`
    Code
      dplyr::show_query(dplyr::slice_min(dplyr::group_by(lf, g), x, with_ties = FALSE))
    Output
      <SQL>
      SELECT `bigrquery_slice`.`g`, `bigrquery_slice`.`x`, `bigrquery_slice`.`y`
      FROM (
        SELECT ARRAY_AGG(`bigrquery_row` ORDER BY `bigrquery_row`.`bigrquery_order` LIMIT 1) AS `bigrquery_rows`
        FROM (
      SELECT *, `x` AS `bigrquery_order`
      FROM `df`
      WHERE (NOT((`x` IS NULL)))
        ) AS `bigrquery_row`
        GROUP BY `bigrquery_row`.`g`
      ), UNNEST(`bigrquery_rows`) AS `bigrquery_slice`
    Code
      dplyr::show_query(dplyr::slice_sample(lf, n = 1, by = g))
    Output
      <SQL>
      SELECT `bigrquery_slice`.`g`, `bigrquery_slice`.`x`, `bigrquery_slice`.`y`
      FROM (
        SELECT ARRAY_AGG(`bigrquery_row` ORDER BY RAND() LIMIT 1) AS `bigrquery_rows`
        FROM (
      SELECT *
      FROM `df`
        ) AS `bigrquery_row`
        GROUP BY `bigrquery_row`.`g`
      ), UNNEST(`bigrquery_rows`) AS `bigrquery_slice`
    Code
      dplyr::show_query(dplyr::slice_min(lf, x + y, with_ties = FALSE))
    Output
      <SQL>
      SELECT `bigrquery_row`.`g`, `bigrquery_row`.`x`, `bigrquery_row`.`y`
      FROM (
      SELECT *
      FROM (
        SELECT *, `x` + `y` AS `bigrquery_order`
        FROM `df`
      ) AS `q01`
      WHERE (NOT((`bigrquery_order` IS NULL)))
      ) AS `bigrquery_row`
      ORDER BY `bigrquery_row`.`bigrquery_order`
      LIMIT 1

# slice_*() fall back to window functions otherwise

    Code
      dplyr::slice_sample(lf, n = 1, weight_by = x)
    Condition
      Error in `dplyr::slice_sample()`:
      ! Weighted resampling is not supported on database backends

