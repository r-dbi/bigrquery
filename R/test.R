#' Project to use for testing bigquery
#'
#' You'll need to set up the `BIGQUERY_TEST_PROJECT` env var if you want to
#' run bigquery tests locally. I recommend creating a new project because
#' the tests involve both reading and writing. You will also need to
#' have billing billing enabled for the project.
#'
#' @section Testing:
#' In tests, `bq_test_project()` (and hence `bq_test_dataset()`) will
#' automatically skip if auth and a test project are not available.
#'
#' @return `bq_test_project()` returns the name of a project suitable for
#'   use in testing. `bq_test_dataset()` creates a temporary dataset
#'   who's lifetime is tied to the lifetime of the object that it returns.
#' @export
#' @keywords internal
#' @examples
#' if (bq_testable()) {
#'   ds <- bq_test_dataset()
#'   bq_mtcars <- bq_table_upload(bq_table(ds, "mtcars"), mtcars)
#'
#'   # dataset and table will be automatically deleted when ds is GC'd
#' }
bq_test_project <- function() {
  if (is_testing() && !bq_authable()) {
    testthat::skip("No BigQuery access credentials availabe")
  }

  env <- Sys.getenv("BIGQUERY_TEST_PROJECT")
  if (!identical(env, "")) {
    return(env)
  }

  if (is_testing()) {
    testthat::skip("BIGQUERY_TEST_PROJECT not set")
  }

  stop(
    "To run bigquery tests you must have BIGQUERY_TEST_PROJECT envvar set ",
    "to name of project which has billing set up and to which you have ",
    "right access",
    call. = FALSE
  )
}

#' @export
#' @rdname bq_test_project
bq_test_dataset <- function() {
  ds <- bq_dataset(bq_test_project(), random_name())
  bq_dataset_create(ds)

  env <- new.env()
  reg.finalizer(
    env,
    function(e) bq_dataset_delete(ds, recursive = TRUE),
    onexit = TRUE
  )
  attr(ds, "env") <- env

  ds
}

#' @export
#' @rdname bq_test_project
bq_testable <- function() {
  bq_authable() && !identical(Sys.getenv("BIGQUERY_TEST_PROJECT"), "")
}

#' @export
#' @rdname bq_test_project
bq_authable <- function() {
  has_access_cred() || (interactive() && !is_testing())
}

random_name <- function(n = 10) {
  paste0("TEMP_", sample(letters, n, replace = TRUE), collapse = "")
}

is_testing <- function() identical(Sys.getenv("TESTTHAT"), "true")
