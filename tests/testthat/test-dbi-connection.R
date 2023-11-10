test_that("can connect and disconnect", {
  con <- DBI::dbConnect(bigquery(), project = "p")
  expect_true(dbIsValid(con))

  expect_true(dbDisconnect(con))
  # But still valid, because there's actually no long-lived connection
  expect_true(dbIsValid(con))
})

test_that("can round-trip connection to bq_dataset", {
  ds <- bq_dataset("x", "y")
  con <- DBI::dbConnect(ds)
  expect_equal(as_bq_dataset(con), ds)
})

test_that("useful print with and without dataset", {
  con1 <- DBI::dbConnect(bigquery(), project = "p", dataset = "x", billing = "b")
  con2 <- DBI::dbConnect(bigquery(), project = "p")

  expect_snapshot({
    "With dataset"
    con1

    "Without dataset"
    con2
  })
})

test_that("uses BigQuery quoting standards", {
  con <- DBI::dbConnect(bigquery(), project = "p")

  expect_equal(DBI::dbQuoteString(con, "x"), DBI::SQL("'x'"))
  expect_equal(DBI::dbQuoteIdentifier(con, "x"), DBI::SQL("`x`"))

  # Doesn't double-quote
  x <- SQL("x")
  expect_equal(DBI::dbQuoteString(con, x), x)
  expect_equal(DBI::dbQuoteIdentifier(con, x), x)

  # Returns 0-length outputs
  expect_equal(DBI::dbQuoteString(con, character()), SQL(character()))
  expect_equal(DBI::dbQuoteIdentifier(con, character()), SQL(character()))
})

test_that("dbQuoteIdentifier validates inputs", {
  con <- DBI::dbConnect(bigquery(), project = "")
  expect_snapshot(DBI::dbQuoteIdentifier(con, c("x", NA)), error = TRUE)
})

test_that("can retrieve information about public dataset", {
  con <- DBI::dbConnect(
    bigquery(),
    project = "publicdata",
    dataset = "samples",
    billing = bq_test_project()
  )

  fields <- DBI::dbListFields(con, "natality")
  expect_length(fields, 31)

  expect_true("natality" %in% DBI::dbListTables(con))

  df <- DBI::dbReadTable(con, "natality", n_max = 10)
  expect_equal(nrow(df), 10)
})

test_that("can roundtrip a data frame", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(ds)

  DBI::dbWriteTable(con, "mtcars", mtcars)
  df <- DBI::dbReadTable(con, "mtcars")

  expect_equal(nrow(df), 32)
  expect_equal(ncol(df), 11)
})

test_that("dbWriteTable errors on unsupported arguments", {
  con <- DBI::dbConnect(bigquery(), project = "")
  df <- data.frame(x = 1)

  expect_snapshot(error = TRUE, {
    DBI::dbWriteTable(con, "x", df, field.types = list())
    DBI::dbWriteTable(con, "x", df, temporary = TRUE)
  })

})

test_that("can execute a query", {
  tb <- bq_test_table()
  con <- DBI::dbConnect(bq_dataset(tb$project, tb$dataset))

  DBI::dbWriteTable(con, tb$table, data.frame(x = 1:4))
  out <- dbExecute(con, sprintf("UPDATE %s SET x = x + 1 WHERE true", tb$table))
  expect_equal(out, 4)

  out <- dbExecute(con, sprintf("DELETE %s WHERE x <= 3", tb$table))
  expect_equal(out, 2)
})

test_that("can use parameters", {
  con <- DBI::dbConnect(bigquery(), project = bq_test_project())

  df <- DBI::dbGetQuery(con, "SELECT @x AS value", params = list(x = 1))
  expect_equal(df, tibble(value = 1))
})

test_that("can use DBI::Id()", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(ds)

  df <- data.frame(x = 1:10)
  id <- DBI::Id(table = "mytable")

  expect_no_error(DBI::dbCreateTable(con, id, df))
  expect_no_error(DBI::dbAppendTable(con, id, df))
  expect_no_error(DBI::dbWriteTable(con, id, df, overwrite = TRUE))
  expect_no_error(DBI::dbReadTable(con, id))
  expect_true(DBI::dbExistsTable(con, id))
  expect_no_error(DBI::dbListFields(con, id))
  expect_no_error(DBI::dbRemoveTable(con, id))
})

test_that("can create an empty dataset then append to it", {
  tb <- bq_test_table()
  con <- DBI::dbConnect(bq_dataset(tb$project, tb$dataset))

  df <- data.frame(x = 1, y = 2)
  DBI::dbCreateTable(con, tb$table, df)
  expect_equal(bq_table_nrow(tb), 0)

  # With dbWriteTable
  DBI::dbWriteTable(con, tb$table, df, append = TRUE)
  expect_equal(bq_table_nrow(tb), 1)

  # Or with dbAppend
  DBI::dbAppendTable(con, tb$table, df)
  expect_equal(bq_table_nrow(tb), 2)
})

test_that("dataset is optional", {
  con <- DBI::dbConnect(bigquery(), project = bq_test_project())
  expect_snapshot(DBI::dbListTables(con), error = TRUE)

  df <- DBI::dbReadTable(con, "publicdata.samples.natality", n_max = 10)
  expect_equal(ncol(df), 31)

  expect_snapshot(DBI::dbReadTable(con, "natality", n_max = 10), error = TRUE)
})

test_that("can create bq_table from connection + name", {
  con1 <- DBI::dbConnect(bigquery(), project = "p")
  expect_snapshot(as_bq_table(con1, "x"), error = TRUE)
  expect_equal(as_bq_table(con1, "x.y"), as_bq_table("p.x.y"))
  expect_equal(as_bq_table(con1, "x.y.z"), as_bq_table("x.y.z"))
  expect_snapshot(as_bq_table(con1, "a.b.c.d"), error = TRUE)

  con2 <- DBI::dbConnect(bigquery(), project = "p", dataset = "d")
  expect_equal(as_bq_table(con2, "x"), as_bq_table("p.d.x"))
  expect_equal(as_bq_table(con2, "x.y"), as_bq_table("p.x.y"))
  expect_equal(as_bq_table(con2, "x.y.z"), as_bq_table("x.y.z"))
})

test_that("can create bq_table() from connection + Id", {
  con1 <- DBI::dbConnect(bigquery(), project = "p")
  expect_equal(
    as_bq_table(con1, Id(schema = "x", table = "y")),
    as_bq_table("p.x.y")
  )
  expect_equal(
    as_bq_table(con1, Id(catalog = "q", schema = "x", table = "y")),
    as_bq_table("q.x.y")
  )

  con2 <- DBI::dbConnect(bigquery(), project = "p", dataset = "d")
  expect_equal(as_bq_table(con2, Id(table = "x")), as_bq_table("p.d.x"))
  expect_equal(
    as_bq_table(con2, Id(schema = "x", table = "y")),
    as_bq_table("p.x.y")
  )
  expect_equal(
    as_bq_table(con2, Id(catalog = "q", schema = "x", table = "y")),
    as_bq_table("q.x.y")
  )
})

test_that("as_bq_table checks its input types", {
  con1 <- DBI::dbConnect(bigquery(), project = "p")
  expect_snapshot(as_bq_table(con1, letters), error = TRUE)
})

test_that("the return type of integer columns is set by the bigint argument", {
  x <- c("-2147483648", "-2147483647", "-1", "0", "1", "2147483647", "2147483648")
  sql <- paste0("SELECT * FROM UNNEST ([", paste0(x, collapse = ","), "]) AS x");

  con_integer64 <- DBI::dbConnect(bigquery(), project = bq_test_project(), bigint = "integer64")
  con_character <- DBI::dbConnect(bigquery(), project = bq_test_project(), bigint = "character")

  expect_equal(DBI::dbGetQuery(con_integer64, sql)$x, bit64::as.integer64(x))
  expect_equal(DBI::dbGetQuery(con_character, sql)$x, x)
})

test_that("DBI::sqlData return TRUE, FALSE for logical class", {
  con <- dbConnect(bigquery(), project = bq_test_project())
  expect_equal(
    DBI::dbQuoteLiteral(con, c(TRUE, FALSE, NA)),
    DBI::SQL(c("TRUE", "FALSE", "NULL"))
  )
})
