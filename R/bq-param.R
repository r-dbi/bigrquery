

bq_param <- function(value, type = NULL, name = NULL) {
  if (is.null(type)) {
    type <- data_type(value)
  }

  if (length(value) > 1) {
    bq_param_array(value, type, name)
  } else {
    bq_param_scalar(value, type, name)
  }
}

#' Defines parameter that can be passed to the query
#'
#' @name bq-param
#' @export
#'
#' @param value vector of parameter values
#' @param name name of the parameter in the query, omitting the `@`
#' @param type BigQuery type of the parameter
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


#' @name bq-param
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

#' Parameters of STRUCT type are not supported
#'
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

  values <- c(x$value)
  values <- lapply(x$value, function(x) list(value = unbox(x)))
  list(
    name = x$name,
    parameterType = list(
      type = "ARRAY",
      arrayType = list(type = unbox(x$type))
    ),
    parameterValue = list(arrayValues = values)
  )
}
