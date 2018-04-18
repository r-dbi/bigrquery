context("bq-table.R")

test_that("can create and delete tables", {
  ds <- bq_test_dataset()

  bq_mtcars <- bq_table(ds, "mtcars")
  expect_false(bq_table_exists(bq_mtcars))

  bq_table_create(bq_mtcars, mtcars)
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

test_that("can round trip a simple data frame", {
  ds <- bq_test_dataset()

  df1 <- tibble(x = 1:10, y = letters[1:10])

  bq_df <- bq_table(ds, "df")
  bq_table_upload(bq_df, df1)

  df2 <- bq_table_download(bq_df)
  df2 <- df2[order(df2$x), names(df1)] # BQ doesn't gaurantee order
  rownames(df2) <- NULL

  expect_equal(df1, df2)
})

test_that("can round trip data frame with list-cols", {
  ds <- bq_test_dataset()
  tb <- bq_table(ds, "complex")

  df1 <- tibble::tibble(
    val = 1,
    array = list(1:5),
    struct = list(list(x = 1, y = 2)),
    array_struct = list(tibble::tibble(x = "a", y = "b"))
  )
  bq_table_upload(tb, df1)

  df2 <- bq_table_download(tb)
  # restore column order
  df2 <- df2[names(df1)]
  df2$struct[[1]] <- df2$struct[[1]][c("x", "y")]
  df2$array_struct[[1]] <- df2$array_struct[[1]][c("x", "y")]

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
