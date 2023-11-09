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

