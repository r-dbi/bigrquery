#' Build a request for the Google BigQuery API
#'
#' @description Build a request, using knowledge of the [BigQuery v2
#'   API](https://cloud.google.com/bigquery/docs/reference/rest/v2/) from its
#'   [Discovery
#'   Document](https://www.googleapis.com/discovery/v1/apis/drive/v3/rest). Most
#'   users should, instead, use higher-level wrappers that facilitate common
#'   tasks, such as EXCITING_TASK_X or USEFUL_TASK_Y. The functions here are
#'   intended for internal use and for programming around the BigQuery API.
#'
#' @description `request_generate()` lets you provide the bare minimum of input.
#'   It takes a nickname for an endpoint and:
#'   * Uses the API spec to look up the `path`, `method`, and base URL.
#'   * Checks `params` for validity and completeness with respect to the
#'   endpoint. Separates parameters into those destined for the body, the query,
#'   and URL endpoint substitution (which is also enacted).
#'
#' @param endpoint Character. Nickname for one of the selected BigQuery v2 API
#'   endpoints built into bigrquery. Learn more in [bq_endpoints()].
#' @param params Named list. Parameters destined for endpoint URL substitution,
#'   the query, or the body.
#' @param token A suitably prepared OAuth2 token.
#'
#' @return `list()`\cr Components are `method`, `path`, `query`, `body`,
#'   `token`, and `url`, suitable as input for [request_make()].
#' @export
#' @family low-level API functions
#' @seealso [gargle::request_develop()], [gargle::request_build()]
#' @examples
#' \dontrun{
#' ## FIXME: make this a BigQuery example
#' req <- request_generate(
#'   "drive.files.get",
#'   list(fileId = "abc"),
#'   token = drive_token()
#' )
#' req
#' }
request_generate <- function(endpoint = character(),
                             params = list(),
                             token = bq_token()) {
  ept <- .endpoints[[endpoint]]
  if (is.null(ept)) {
    stop_glue("\nEndpoint not recognized:\n  * {endpoint}")
  }

  req <- gargle::request_develop(endpoint = ept, params = params)
  gargle::request_build(
    path = req$path,
    method = req$method,
    params = req$params,
    body = req$body,
    token = token
  )
}
