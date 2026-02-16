test_that("bq_perform_upload creates job that succeeds", {
  withr::local_options(cli.progress_show_after = 10)

  bq_mtcars <- bq_test_table()
  job <- bq_perform_upload(bq_mtcars, mtcars)

  expect_s3_class(job, "bq_job")
  expect_snapshot({
    bq_job_wait(job, quiet = FALSE)
    bq_job_wait(job, quiet = FALSE)
  })

  expect_true(bq_table_exists(bq_mtcars))
})

test_that("bq_perform_upload preserves microsecond precision", {
  tbl <- data.frame(psx = .POSIXct(0.123456))
  attr(tbl$psx, "tzone") <- "UTC"

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  bq_table_upload(ds, tbl)
  tbl2 <- bq_table_download(ds)

  expect_equal(tbl$psx, tbl2$psx)
})

test_that("bq_perform_upload correclty assigns tzone", {
  # the bug only produced problems when the system TZ has a non-zero
  # offset (i.e., not UTC)
  withr::local_envvar(TZ = "America/New_York")

  ds <- bq_table(bq_test_project(), "basedata", "datatypes")
  defer(try(bq_table_delete(ds), silent = TRUE))

  df <- data.frame(
    null = .POSIXct(0, tz = NULL),
    utc = .POSIXct(0, tz = "UTC"),
    ny = .POSIXct(0, tz = "America/New_York"),
    nz = .POSIXct(0, tz = "Pacific/Auckland")
  )
  bq_table_upload(ds, df)
  db <- bq_table_download(ds)

  expect_equal(db$utc, .POSIXct(0, tz = "UTC"))
  expect_equal(db$null, .POSIXct(0, tz = "UTC"))
  expect_equal(db$ny, .POSIXct(0, tz = "UTC"))
  expect_equal(db$nz, .POSIXct(0, tz = "UTC"))
})

test_that("bq_perform_copy creates job that succeeds", {
  withr::local_options(cli.progress_show_after = 10)

  src <- as_bq_table("bigquery-public-data.moon_phases.moon_phases")
  dst <- bq_test_table()

  job <- bq_perform_copy(src, dst)
  expect_s3_class(job, "bq_job")

  # Doesn't return any statistics to show
  expect_snapshot({
    bq_job_wait(job, quiet = FALSE)
  })

  expect_true(bq_table_exists(dst))
})


# Load / extract ----------------------------------------------------------

test_that("can round trip extract + load", {
  ds_public <- bq_dataset("bigquery-public-data", "moon_phases")

  tb <- bq_dataset_query(
    ds_public,
    query = "SELECT COUNT(*) as count FROM moon_phases",
    billing = bq_test_project()
  )

  tmp <- gs_test_object()
  # on.exit(gs_object_delete(tmp))

  job <- bq_perform_extract(tb, tmp)
  bq_job_wait(job)

  tb_ks <- bq_test_table()
  job <- bq_perform_load(tb_ks, tmp)
  bq_job_wait(job)

  df <- bq_table_download(tb_ks)
  expect_equal(nrow(df), 1)
  expect_named(df, "count")
})

# Queries -----------------------------------------------------------------

test_that("bq_perform_query creates job that succeeds", {
  ds <- as_bq_dataset("bigquery-public-data.moon_phases")
  job <- bq_perform_query(
    "SELECT count(*) FROM moon_phases",
    billing = bq_test_project(),
    default_dataset = ds
  )

  expect_s3_class(job, "bq_job")
  expect_snapshot({
    bq_job_wait(job, quiet = FALSE)
  })

  job_tb <- bq_job_table(job)
  expect_true(bq_table_exists(job_tb))
})

test_that("can supply scalar parameters", {
  job <- bq_project_query(
    bq_test_project(),
    "SELECT 1 + @x",
    parameters = list(x = bq_param_scalar(1))
  )
  df <- bq_table_download(job)
  expect_setequal(df[[1]], 2)
})

test_that("can supply array parameters", {
  job <- bq_project_query(
    bq_test_project(),
    "SELECT values FROM UNNEST(@x) values",
    parameters = list(x = bq_param_array(c("a", "b")))
  )
  df <- bq_table_download(job)
  expect_setequal(df$values, c("a", "b"))
})

test_that("can estimate cost and get schema", {
  cost <- bq_perform_query_dry_run(
    "SELECT count(*) FROM bigquery-public-data.moon_phases.moon_phases",
    billing = bq_test_project()
  )
  expect_equal(cost, structure(0, class = "bq_bytes"))

  schema <- bq_perform_query_schema(
    "SELECT * FROM bigquery-public-data.moon_phases.moon_phases",
    billing = bq_test_project()
  )
  names <- vapply(schema, function(x) x$name, character(1))
  expect_equal(names, c("phase", "phase_emoji", "peak_datetime"))

  types <- vapply(schema, function(x) x$type, character(1))
  expect_equal(types, c("STRING", "STRING", "DATETIME"))
})
