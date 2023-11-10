#' Project to use for testing bigrquery
#'
#' @description
#' You'll need to set the `BIGQUERY_TEST_PROJECT` (name of a project) and
#' `BIGQUERY_TEST_BUCKET` (name of bucket) env vars in order to run bigrquery
#' tests locally. I recommend creating a new project because the tests involve
#' both reading and writing in BigQuery and Cloud Storage.
#'
#' The `BIGQUERY_TEST_PROJECT` must have billing enabled for the project. While
#' logged in, via `bq_auth()`, as a user with permission to work in
#' `BIGQUERY_TEST_PROJECT`, run `bq_test_init()` once to perform some setup.
#'
#' @section
#' Testing: In tests, `bq_test_project()` (and hence `bq_test_dataset()`) will
#' automatically skip if auth and a test project are not available.
#'
#' @return `bq_test_project()` returns the name of a project suitable for use in
#'   testing. `bq_test_dataset()` creates a temporary dataset whose lifetime is
#'   tied to the lifetime of the object that it returns.
#' @export
#' @keywords internal
#' @examplesIf bq_testable()
#' ds <- bq_test_dataset()
#' bq_mtcars <- bq_table_upload(bq_table(ds, "mtcars"), mtcars)
#'
#' # dataset and table will be automatically deleted when ds is GC'd
bq_test_project <- function() {
  if (is_testing() && !bq_authable()) {
    testthat::skip("No BigQuery access credentials available")
  }

  env <- Sys.getenv("BIGQUERY_TEST_PROJECT")
  if (!identical(env, "")) {
    return(env)
  }

  if (is_testing()) {
    testthat::skip("BIGQUERY_TEST_PROJECT not set")
  } else {
    cli::cli_abort(
      "{.envvar BIGQUERY_TEST_PROJECT} envvar must to set to a project name."
    )
  }
}

#' @export
#' @rdname bq_test_project
#' @param name Dataset name - used only for testing.
bq_test_init <- function(name = "basedata") {
  proj <- bq_test_project()

  basedata <- bq_dataset(proj, name)
  if (!bq_dataset_exists(basedata)) {
    bq_dataset_create(basedata)
  }

  bq_mtcars <- bq_table(basedata, "mtcars")
  if (!bq_table_exists(bq_mtcars)) {
    job <- bq_table_upload(bq_mtcars, values = datasets::mtcars)
  }
}

#' @export
#' @rdname bq_test_project
bq_test_dataset <- function(name = random_name(), location = "US") {
  ds <- bq_dataset(bq_test_project(), name)
  bq_dataset_create(ds, location = location)

  env <- new.env()
  reg.finalizer(
    env,
    function(e) bq_dataset_delete(ds, delete_contents = TRUE),
    onexit = TRUE
  )
  attr(ds, "env") <- env

  ds
}

bq_test_table <- function() {
  ds <- env_cache(the, "test_dataset", bq_test_dataset())
  bq_table(ds, random_name())
}

#' @export
#' @rdname bq_test_project
bq_testable <- function() {
  bq_authable() && !identical(Sys.getenv("BIGQUERY_TEST_PROJECT"), "")
}

#' @export
#' @rdname bq_test_project
bq_authable <- function() {
  bq_has_token() || (is_interactive() && !is_testing())
}

#' @export
#' @rdname bq_test_project
gs_test_bucket <- function() {
  env <- Sys.getenv("BIGQUERY_TEST_BUCKET")
  if (!identical(env, "")) {
    return(env)
  }

  if (is_testing()) {
    testthat::skip("BIGQUERY_TEST_BUCKET not set")
  } else {
    cli::cli_abort(
      "{.envvar BIGQUERY_TEST_BUCKET} must be set to a bucket name."
    )
  }
}


#' @export
#' @rdname bq_test_project
gs_test_object <- function(name = random_name()) {
  gs_object(gs_test_bucket(), name)
}


random_name <- function(n = 10) {
  paste0("TESTING_", paste(sample(letters, n, replace = TRUE), collapse = ""))
}

is_testing <- function() identical(Sys.getenv("TESTTHAT"), "true")

is_snapshot <- function() identical(Sys.getenv("TESTTHAT_IS_SNAPSHOT"), "true")

skip_if_no_auth <- function() {
  testthat::skip_if_not(bq_has_token(), "Authentication not available")
}
