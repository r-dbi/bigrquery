
bq_param <- function(value, type = NULL) {
  assert_that(length(value) > 0)

  if (is.null(type)) {
    type <- data_type(value)
  }

  structure(
    list(value = value, type = type),
    class = "bq_param"
  )
}

as_bq_param <- function(x) {
  if (inherits(x, "bq_param")) {
    x
  } else {
    bq_param(x)
  }
}

bq_params <- function(x) {
  structure(x, class = "bq_params")
}

as_bq_params <- function(x) {
  bq_params(lapply(x, as_bq_param))
}

#' @export
as_json.bq_params <- function(x) {
  json <- Map(as_json_bq_param, x, names(x))
  unname(json)
}


#' Parameters of STRUCT type are not supported
#'
#' @noRd
as_json_bq_param <- function(x, name) {
  if (bq_param_is_scalar(x)) {
    list(
      name = name,
      parameterType = list(type = unbox(x$type)),
      parameterValue = list(value = unbox(x$value))
    )
  }
  else {
    values <- c(x$value)
    values <- lapply(x$value, function(x) list(value = unbox(x)))
    list(
      name = name,
      parameterType = list(
        type = "ARRAY",
        arrayType = list(type = unbox(x$type))
      ),
      parameterValue = list(arrayValues = values)
    )
  }
}

bq_param_is_scalar <- function(x) {
  length(x$value) == 1L
}
