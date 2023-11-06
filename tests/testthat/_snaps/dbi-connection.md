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

