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
