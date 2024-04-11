# can retrieve query in pieces and that quiet is respected

    Code
      DBI::dbFetch(res, NA)
    Condition
      Error in `DBI::dbFetch()`:
      ! is.numeric(n) is not TRUE

---

    Code
      df <- DBI::dbFetch(res, 10)

# can get metadata

    Code
      res
    Output
      <BigQueryResult>
        Query: SELECT cyl, mpg FROM mtcars
        Has completed: FALSE
        Rows fetched: 0

