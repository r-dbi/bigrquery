dots <- function(...) {
  eval(substitute(alist(...)))
}

named_dots <- function(...) {
  args <- dots(...)

  nms <- names2(args)
  missing <- nms == ""
  if (all(!missing)) return(args)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(args[missing], deparse2, character(1), USE.NAMES = FALSE)

  names(args)[missing] <- defaults
  args
}

as_df <- function(x) {
  class(x) <- "data.frame"
  attr(x, "row.names") <- c(NA_integer_, -length(x[[1]]))

  x
}

timer <- function() {
  start <- proc.time()[[3]]
  function() {
    proc.time()[[3]] - start
  }
}

"%||%" <- function(x, y) if(is.null(x)) y else x

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

# Given a string and a separator, split the string at the rightmost
# occurrence of the separator and return the two parts.
rsplit_one <- function(str, sep) {
  assert_that(is.string(str), is.string(sep))
  parts <- strsplit(str, sep, fixed = TRUE)[[1]]
  right <- parts[length(parts)]
  if (length(parts) > 1) {
    left <- paste0(parts[-length(parts)], collapse = sep)
  } else {
    left <- NULL
  }
  list(left = left, right = right)
}

#' Parse a BQ-style identifier into project/dataset IDs.
#'
#' @param dataset dataset name
#' @param project_id (optional) project ID to use if none is provided
#' in \code{dataset}
#' @return a list with \code{project_id} and \code{dataset_id} components
#' (either of which may be \code{NULL}).
#' @export
parse_dataset <- function(dataset, project_id = NULL) {
  assert_that(is.string(dataset), is.null(project_id) || is.string(project_id))
  first_split <- rsplit_one(dataset, ":")
  dataset_id <- first_split$right
  project_id <- first_split$left %||% project_id
  list(project_id = project_id, dataset_id = dataset_id)
}

#' Parse a BQ-style identifier into project/dataset/table IDs.
#'
#' @param table table name
#' @param project_id (optional) project ID to use if none is provided
#' in \code{table}
#' @return a list with \code{project_id}, \code{dataset_id}, and
#' \code{table_id} components (any of which may be \code{NULL}).
#' @export
parse_table <- function(table, project_id = NULL) {
  assert_that(is.string(table), is.null(project_id) || is.string(project_id))
  dataset_id <- NULL
  first_split <- rsplit_one(table, ".")
  table_id <- first_split$right
  project_and_dataset <- first_split$left
  if (!is.null(project_and_dataset)) {
    second_split <- rsplit_one(project_and_dataset, ":")
    dataset_id <- second_split$right
    project_id <- second_split$left %||% project_id
  }
  list(project_id = project_id, dataset_id = dataset_id, table_id = table_id)
}
