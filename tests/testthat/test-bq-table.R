test_that("can create and delete tables", {
  ds <- bq_test_dataset()

  bq_mtcars <- bq_table(ds, "mtcars")
  expect_false(bq_table_exists(bq_mtcars))

  bq_table_create(
    bq_mtcars,
    mtcars,
    friendly_name = "Motor Trend Car Road Tests",
    description = "The data was extracted from the 1974 Motor Trend US magazine",
    labels = list(category = "test")
  )
  expect_true(bq_table_exists(bq_mtcars))

  bq_table_delete(bq_mtcars)
  expect_false(bq_table_exists(bq_mtcars))
})

test_that("can retrieve table size information", {
  bq_mtcars <- bq_table(bq_test_project(), "basedata", "mtcars")
  expect_equal(bq_table_nrow(bq_mtcars), 32)
  expect_equal(as.numeric(bq_table_size(bq_mtcars)), 2816)
})

test_that("can create table with schema", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "df")

  df <- data.frame(x = 1, y = "a")
  bq_table_create(tb, "df", fields = df)

  fields <- bq_table_fields(tb)
  expect_equal(fields, as_bq_fields(df))
})

test_that("can round trip to non-default location", {
  asia <- bq_test_dataset(location = "asia-east1")
  df1 <- tibble(x = 1:10, y = letters[1:10])

  bq_df <- bq_table(asia, "df")
  bq_table_upload(bq_df, df1)

  df2 <- bq_table_download(bq_df)
  df2 <- df2[order(df2$x), names(df1)] # BQ doesn't guarantee order
  rownames(df2) <- NULL

  expect_equal(df1, df2)
})

test_that("can roundtrip via save + load", {
  ds <- bq_test_dataset()

  tb1 <- bq_table(bq_test_project(), "basedata", "mtcars")
  tb2 <- bq_table(ds, "save_load")
  gs <- gs_test_object()

  bq_table_save(tb1, gs)
  on.exit(gs_object_delete(gs))
  bq_table_load(tb2, gs)

  df <- bq_table_download(tb2)
  expect_equal(dim(df), c(32, 11))
})

test_that("can copy table from public dataset", {
  ds <- bq_test_dataset()
  my_natality <- bq_table(ds, "mynatality")

  out <- bq_table_copy("publicdata.samples.natality", my_natality)
  expect_equal(out, my_natality)
  expect_true(bq_table_exists(my_natality))
})

# data-types --------------------------------------------------------------

test_that("can round trip atomic vectors", {
  ds <- bq_test_dataset()
  df1 <- tibble(
    lgl = c(FALSE, TRUE, NA),
    int = c(-1, 1, NA),
    dbl = c(-1.5, 1.5, NA),
    chr = c("A", "B", NA)
  )

  bq_df <- bq_table(ds, "df")
  bq_table_upload(bq_df, df1)

  df2 <- bq_table_download(bq_df, bigint = "integer")
  df2 <- df2[order(df2[[1]]), names(df1)] # BQ doesn't gaurantee order
  rownames(df2) <- NULL

  expect_equal(df1, df2)
})

test_that("can round-trip POSIXt to either TIMESTAMP or DATETIME", {
  ds <- bq_test_dataset()
  df <- tibble(datetime = as.POSIXct("2020-01-01 09:00", tz = "UTC"))

  tb1 <- bq_table_create(
    bq_table(ds, "timestamp"),
    bq_fields(list(bq_field("datetime", "TIMESTAMP")))
  )
  bq_table_upload(tb1, df)
  df1 <- bq_table_download(tb1)
  expect_equal(df1, df)

  tb2 <- bq_table_create(
    bq_table(ds, "datetime2"),
    bq_fields(list(bq_field("datetime", "DATETIME")))
  )
  bq_table_upload(tb2, df)
  df2 <- bq_table_download(tb2)
  expect_equal(df2, df)
})

test_that("can round trip data frame with list-cols", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "complex")

  df1 <- tibble::tibble(
    val = 1.5,
    array = list(1L:5L),
    struct = list(list(x = "a", y = 1.5, z = 2L)),
    array_struct = list(tibble::tibble(x = "a", y = 1.5, z = 2L))
  )
  bq_table_upload(tb, df1)

  df2 <- bq_table_download(tb, bigint = "integer")
  # restore column order
  df2 <- df2[names(df1)]
  df2$struct[[1]] <- df2$struct[[1]][c("x", "y", "z")]
  df2$array_struct[[1]] <- df2$array_struct[[1]][c("x", "y", "z")]

  # Converting to dataframe to avoid getting the error:
  # Can't join on 'array' x 'array' because of incompatible types (list / list)
  df1 <- as.data.frame(df1)
  df2 <- as.data.frame(df2)
  expect_equal(df1, df2)
})

test_that("can create table field description", {
  ds <- bq_test_dataset()
  partition_table <- bq_table(ds, "table_field_description")

  bq_table_create(
    partition_table,
    fields = bq_fields(list(bq_field("id", "integer", description = "Key field")))
  )

  meta <- bq_table_meta(partition_table)
  expect_equal(meta$schema$fields[[1]]$description, "Key field")
})

test_that("can patch table with new fields in the schema", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "table_to_patch")
  df <- data.frame(id = 1)
  bq_table_create(tb, fields = df)

  df.patch <- data.frame(id = 1, title = "record name")
  bq_table_patch(tb, fields = df.patch)

  tb.meta <- bq_table_meta(tb)
  expect_equal(
    tb.meta$schema$fields[[2]]$name,
    "title"
  )
})

test_that("can round-trip GEOGRAPHY", {
  skip_if_not_installed("wk")

  ds <- bq_test_dataset()
  df <- tibble(geography = wk::wkt("POINT(30 10)"))

  tb1 <- bq_table_create(
    bq_table(ds, "geography"),
    as_bq_fields(df)
  )
  bq_table_upload(tb1, df)
  df1 <- bq_table_download(tb1)
  expect_equal(df1, df)
})

test_that("can round-trip BYTES", {
  ds <- bq_test_dataset()
  df <- tibble(x = blob::blob(charToRaw("hi!"), charToRaw("bye")))

  tb1 <- bq_table_create(
    bq_table(ds, "bytes"),
    as_bq_fields(df)
  )
  bq_table_upload(tb1, df)
  df1 <- bq_table_download(tb1)
  expect_equal(df1, df)
})
