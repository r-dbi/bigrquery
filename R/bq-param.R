
bq_param <- function(value, type = NULL) {
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

  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- list(
      name = names(x)[[i]],
      parameterType = list(type = unbox(x[[i]]$type)),
      parameterValue = list(value = x[[i]]$value)
    )
  }

  out
}
