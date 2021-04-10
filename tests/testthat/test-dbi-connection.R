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

  expect_known_output({
    cat_line("With dataset:")
    print(con1)

    cat_line()
    cat_line("Without dataset:")
    print(con2)
  }, file = test_path("dbi-connection-print.txt"))
})

test_that("uses BigQuery quoting standards", {
  con <-  DBI::dbConnect(bigquery(), project = "p")

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

  df <- DBI::dbReadTable(con, "natality", max_results = 10)
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

test_that("can append to an existing dataset", {
  ds <- bq_test_dataset()
  con <- DBI::dbConnect(ds)

  df <- data.frame(x = 1, y = 2)
  DBI::dbWriteTable(con, "df", df)
  DBI::dbWriteTable(con, "df", df, append = TRUE)

  df2 <- DBI::dbReadTable(con, "df")
  expect(nrow(df2), 2L)
})

test_that("dataset is optional", {
  con <- DBI::dbConnect(bigquery(), project = bq_test_project())
  expect_error(DBI::dbListTables(con), "`dataset`")

  df <- DBI::dbReadTable(con, "publicdata.samples.natality", max_results = 10)
  expect_equal(ncol(df), 31)

  expect_error(
    DBI::dbReadTable(con, "natality", max_results = 10),
    "must have 2 or 3 components"
  )
})

test_that("can create bq_table from connection + name", {
  con1 <- DBI::dbConnect(bigquery(), project = "p")
  expect_error(as_bq_table(con1, "x"), "must have 2 or 3 components")
  expect_equal(as_bq_table(con1, "x.y"), as_bq_table("p.x.y"))
  expect_equal(as_bq_table(con1, "x.y.z"), as_bq_table("x.y.z"))
  expect_error(as_bq_table(con1, "a.b.c.d"), "must have 1-3 components")

  con2 <- DBI::dbConnect(bigquery(), project = "p", dataset = "d")
  expect_equal(as_bq_table(con2, "x"), as_bq_table("p.d.x"))
  expect_equal(as_bq_table(con2, "x.y"), as_bq_table("p.x.y"))
  expect_equal(as_bq_table(con2, "x.y.z"), as_bq_table("x.y.z"))
})

test_that("the return type of integer columns is set by the bigint argument", {
  x <- c("-2147483648", "-2147483647", "-1", "0", "1", "2147483647", "2147483648")
  sql <- paste0("SELECT * FROM UNNEST ([", paste0(x, collapse = ","), "]) AS x");

  con_integer64 <- DBI::dbConnect(bigquery(), project = bq_test_project(), bigint = "integer64")
  con_character <- DBI::dbConnect(bigquery(), project = bq_test_project(), bigint = "character")

  expect_equal(DBI::dbGetQuery(con_integer64, sql)$x, bit64::as.integer64(x))
  expect_equal(DBI::dbGetQuery(con_character, sql)$x, x)
})
