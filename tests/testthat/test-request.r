context("request")

test_that("api keys are added when present", {
  tryCatch({
    key <- 'my.secret.key'
    Sys.setenv(BIGRQUERY_API_KEY = key)
    expect_that(bigrquery:::prepare_bq_query(NULL), equals(list(key = key)))
    expect_that(bigrquery:::prepare_bq_query(list(herring_color = "red")),
                equals(list(herring_color = "red", key = key)))
  }, finally = {
    Sys.unsetenv("BIGRQUERY_API_KEY")
  })
})

test_that("explicit api keys override env vars", {
  tryCatch({
    key <- 'my.secret.key'
    Sys.setenv(BIGRQUERY_API_KEY = key)
    expect_that(bigrquery:::prepare_bq_query(list(key = "my.other.key")),
                equals(list(key = "my.other.key")))
  }, finally = {
    Sys.unsetenv("BIGRQUERY_API_KEY")
  })
})
