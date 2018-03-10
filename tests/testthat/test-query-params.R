context("query_exec parameters")


test_that("named parameters can be used", {
  skip_if_no_auth()

  sql <- "SELECT *
          FROM
            (SELECT 1 x
             UNION ALL
             SELECT 2 x)
          WHERE x = @x"
  params <- list(x = 2)

  df <- query_exec(
      sql,
      project = Sys.getenv("BIGQUERY_PROJECT"),
      use_legacy_sql = FALSE,
      parameters = params
    )

  expect_equal(df$x, c(2), label = "Only value matching the filter was selected")

  params <- list(y = 2)

  expect_error(query_exec(sql,
                   project = Sys.getenv("BIGQUERY_PROJECT"),
                   use_legacy_sql = FALSE,
                   parameters = params),
               regexp = "Query parameter.*not found at",
               label = "Error is thrown if wrong parameter name is given")

})


test_that("parameters are converted to correct data structure", {
  params <- list(x = 2L, d = as.Date("2012-10-15"))
  params.list <- bq_parameters(params)
  expected <- list(
    list(
      parameterType = list(type = "INTEGER"),
      parameterValue = list(value = 2L),
      name = "x"
    ),
    list(
      parameterType = list(type = "DATE"),
      parameterValue = list(value = as.Date("2012-10-15")),
      name = "d"
    )
  )
  expect_equal(params.list, expected, label = "Parameters datastructure is correct.")
})
