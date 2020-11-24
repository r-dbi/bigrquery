#' Explicitly define query parameters
#'
#' By default, bigrquery will assume vectors of length 1 are scalars,
#' and longer vectors are arrays. If you need to pass a length-1 array,
#' you'll need to explicitly use `bq_param_array()`.
#'
#' @param value vector of parameter values
#' @param name name of the parameter in the query, omitting the `@`
#' @param type BigQuery type of the parameter
#' @keywords internal
#' @export
#' @examples
#' # bq_param() automatically picks scalar vs array based on length
#' bq_param("a")
#' bq_param(c("a", "b", "c"))
#'
#' # use bq_param_array() to create a length-1 array
#' bq_param_array("a")
bq_param <- function(value, type = NULL, name = NULL) {
  if (length(value) > 1) {
    bq_param_array(value, type, name)
  } else {
    bq_param_scalar(value, type, name)
  }
}

#' @rdname bq_param
#' @export
bq_param_scalar <- function(value, type = NULL, name = NULL) {
  assert_that(length(value) == 1)

  if (is.null(type)) {
    type <- data_type(value)
  }
  structure(
    list(value = value, name = name, type = type),
    class = c("bq_param_scalar", "bq_param")
  )
}

#' @rdname bq_param
#' @export
bq_param_array <- function(value, type = NULL, name = NULL) {
  assert_that(length(value) > 0)

  if (is.null(type)) {
    type <- data_type(value)
  }
  structure(
    list(value = value, name = name, type = type),
    class = c("bq_param_array", "bq_param")
  )
}

as_bq_param <- function(x, name) {
  if (inherits(x, "bq_param")) {
    if (!is.null(name) & is.null(x$name)) {
      x$name <- name
    }
    x
  } else {
    bq_param(name = name, x)
  }
}

bq_params <- function(x) {
  structure(x, class = "bq_params")
}

as_bq_params <- function(x) {
  params <- lapply(names(x), function(name) {
    as_bq_param(x[[name]], name)
  })
  bq_params(params)
}

#' @export
as_json.bq_params <- function(x) {
  json <- lapply(x, as_json)
  unname(json)
}


#' @export
as_json.bq_param_scalar <- function(x) {
  list(
    name = x$name,
    parameterType = list(type = unbox(x$type)),
    parameterValue = list(value = unbox(x$value))
  )
}

#' @export
as_json.bq_param_array <- function(x) {
  values <- unname(c(x$value))
  values <- lapply(values, function(x) list(value = unbox(x)))
  list(
    name = x$name,
    parameterType = list(
      type = "ARRAY",
      arrayType = list(type = unbox(x$type))
    ),
    parameterValue = list(arrayValues = values)
  )
}

#' @export
print.bq_param <- function(x, ...) {
  cat(show_json(as_json(x)))
  invisible(x)
}

#' @export
print.bq_params <- function(x, ...) {
  cat(show_json(as_json(x)))
  invisible(x)
}
