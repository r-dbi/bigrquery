gs_object <- function(bucket, object) {
  structure(
    list(bucket = bucket, object = object),
    class = "gs_object"
  )
}

#' @export
as.character.gs_object <- function(x, ...) {
  format(x)
}

#' @export
format.gs_object <- function(x, ...) {
  sprintf("gs://%s/%s", x$bucket, x$object)
}

#' @export
print.gs_object <- function(x, ...) {
  cat_line("<gs_object> ", format(x))
  invisible(x)
}

gs_object_delete <- function(x, token = bq_token()) {
  url <- sprintf(
    "https://storage.googleapis.com/storage/v1/b/%s/o/%s",
    x$bucket,
    x$object
  )
  req <- request(url)
  req <- req_method(req, "DELETE")
  req <- req_user_agent(req, bq_ua())
  req <- req_auth_bearer_token(req, token$auth_token)
  req_perform(req)
  invisible(NULL)
}

gs_object_exists <- function(x, token = bq_token()) {
  url <- sprintf(
    "https://storage.googleapis.com/storage/v1/b/%s/o/%s",
    x$bucket,
    x$object
  )
  req <- request(url)
  req <- req_user_agent(req, bq_ua())
  req <- req_auth_bearer_token(req, token$auth_token)
  # A 404 is not an error here.
  req <- req_error(req, is_error = function(resp) {
    resp_status(resp) != 404 && resp_status(resp) >= 400
  })
  resp <- req_perform(req)
  resp_status(resp) != 404
}
