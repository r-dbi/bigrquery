#' Project to use for testing bigquery
#'
#' You'll need to set up the `BIGQUERY_TEST_PROJECT` env var if you want to
#' run bigquery tests locally. I recommend creating a new project because
#' the tests involve both reading and writing. You will also need to
#' have billing billing enabled for the project.
#'
#' @export
#' @keywords internal
bq_test_project <- function() {
  env <- Sys.getenv("BIGQUERY_TEST_PROJECT")
  if (!identical(env, "")) {
    return(env)
  }

  stop(
    "To run bigquery tests you must have BIGQUERY_TEST_PROJECT envvar set ",
    "to name of project which has billing set up and to which you have ",
    "right access",
    call. = FALSE
  )
}
