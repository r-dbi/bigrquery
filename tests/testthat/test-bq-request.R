test_that("api keys are added when present", {
  key <- "my.secret.key"
  withr::local_envvar(list(BIGRQUERY_API_KEY = key))

  expect_equal(prepare_bq_query(NULL), list(key = key))
  expect_equal(
    prepare_bq_query(list(herring_color = "red")),
    list(herring_color = "red", key = key)
  )
})

test_that("explicit api keys override env vars", {
  key <- "my.secret.key"
  withr::local_envvar(list(BIGRQUERY_API_KEY = key))

  expect_equal(
    prepare_bq_query(list(key = "my.other.key")),
    list(key = "my.other.key")
  )
})

test_that("pagination warns if pages left on server", {
  skip_if_no_auth()

  expect_warning(
    bq_get_paginated(
      bq_path("bigquery-public-data", ""),
      query = list(fields = "datasets(datasetReference)"),
      page_size = 10,
      max_pages = 2
    ),
    "Only first 20 results"
  )

})

test_that("error call is forwarded all the way down", {
  skip_if_not(bq_authable())

  expect_snapshot(bq_job_meta("a.b.c"), error = TRUE)
})
