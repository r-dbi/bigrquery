# useful print with and without dataset

    Code
      # With dataset
      con1
    Output
      <BigQueryConnection>
        Dataset: p.x
        Billing: b
    Code
      # Without dataset
      con2
    Output
      <BigQueryConnection>
        Billing: p

# dbQuoteIdentifier validates inputs

    Code
      DBI::dbQuoteIdentifier(con, c("x", NA))
    Condition
      Error in `DBI::dbQuoteIdentifier()`:
      ! `x` must not contain missing values.

# dbWriteTable errors on unsupported arguments

    Code
      DBI::dbWriteTable(con, "x", df, field.types = list())
    Condition
      Error in `DBI::dbWriteTable()`:
      ! `field.types` not supported by bigrquery.
    Code
      DBI::dbWriteTable(con, "x", df, temporary = TRUE)
    Condition
      Error in `DBI::dbWriteTable()`:
      ! `temporary = FALSE` not supported by bigrquery.

# dataset is optional

    Code
      DBI::dbListTables(con)
    Condition
      Error in `DBI::dbListTables()`:
      ! Can't list tables without a connection `dataset`.

---

    Code
      DBI::dbReadTable(con, "natality", n_max = 10)
    Condition
      Error in `as_bq_table()`:
      ! `name` ("natality") must have 2 or 3 components if the connection doesn't have a dataset.

# can create bq_table from connection + name

    Code
      as_bq_table(con1, "x")
    Condition
      Error in `as_bq_table()`:
      ! `name` ("x") must have 2 or 3 components if the connection doesn't have a dataset.

---

    Code
      as_bq_table(con1, "a.b.c.d")
    Condition
      Error in `as_bq_table()`:
      ! `name` ("a.b.c.d") must have 1-3 components.

# as_bq_table checks its input types

    Code
      as_bq_table(con1, letters)
    Condition
      Error in `as_bq_table()`:
      ! `name` must be a string or a dbplyr_table_ident.

