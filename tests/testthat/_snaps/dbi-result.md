# can retrieve query in pieces and that quiet is respected

    Code
      df <- DBI::dbFetch(res, 10)
    Message
      Downloading first chunk of data.
      First chunk includes all requested rows.

---

    Code
      df <- DBI::dbFetch(res, -1)

# can get metadata

    Code
      res
    Output
      <BigQueryResult>
        Query: SELECT cyl, mpg FROM mtcars
        Has completed: FALSE
        Rows fetched: 0

