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
