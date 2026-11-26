# registered onLoad

#' Collect a BigQuery table
#'
#' This collect method is specialised for BigQuery tables, generating the
#' SQL from your dplyr commands, then calling [bq_project_query()]
#' or [bq_dataset_query()] to run the query, then [bq_table_download()]
#' to download the results. Thus the arguments are a combination of the
#' arguments to [dplyr::collect()], `bq_project_query()`/`bq_dataset_query()`,
#' and `bq_table_download()`.
#'
#' @inheritParams dplyr::collect
#' @inheritParams bq_table_download
#' @param n Maximum number of results to retrieve.
#'   The default, `Inf`, will retrieve all rows.
#' @param ... Other arguments passed on to
#'   `bq_project_query()`/`bq_project_query()`
#' @method collect tbl_BigQueryConnection
collect.tbl_BigQueryConnection <- function(
  x,
  ...,
  n = Inf,
  api = c("json", "arrow"),
  page_size = NULL,
  max_connections = 6L
) {
  api <- check_api(api)
  check_number_whole(n, min = 0, allow_infinite = TRUE)
  check_number_whole(max_connections, min = 1)

  con <- dbplyr::remote_con(x)
  billing <- con@billing

  if (op_can_download(x)) {
    lq <- x$lazy_query
    name <- op_table(x, con)
    tb <- as_bq_table(con, name)
    n <- min(op_rows(x$lazy_query), n)
  } else {
    sql <- dbplyr::db_sql_render(con, x)

    if (is.null(con@dataset)) {
      tb <- bq_project_query(
        billing,
        sql,
        quiet = con@quiet,
        labels = con@labels,
        ...
      )
    } else {
      ds <- as_bq_dataset(con)
      tb <- bq_dataset_query(
        ds,
        sql,
        quiet = con@quiet,
        billing = billing,
        labels = con@labels,
        ...
      )
    }
  }

  quiet <- if (n < 100) TRUE else con@quiet
  bigint <- con@bigint %||% "integer"

  if (api == "arrow") {
    out <- bq_table_download(
      tb,
      n_max = n,
      quiet = quiet,
      bigint = bigint,
      billing = billing,
      api = "arrow"
    )
  } else {
    out <- bq_table_download(
      tb,
      n_max = n,
      page_size = page_size,
      quiet = quiet,
      max_connections = max_connections,
      bigint = bigint,
      api = "json"
    )
  }

  dplyr::grouped_df(out, intersect(dbplyr::op_grps(x), names(out)))
}

# Can download directly if only head and

op_can_download <- function(x) UseMethod("op_can_download")
#' @export
op_can_download.tbl_lazy <- function(x) op_can_download(x$lazy_query)
#' @export
op_can_download.lazy_query <- function(x) FALSE
#' @export
op_can_download.lazy_select_query <- function(x) {
  query_is_head_only(x)
}
#' @export
op_can_download.lazy_base_query <- function(x) {
  if (isTRUE(x$is_view)) {
    FALSE
  } else if (inherits(x$x, "sql")) {
    FALSE
  } else {
    dbplyr::is.ident(x$x) ||
      inherits(x$x, "dbplyr_table_ident") ||
      inherits(x$x, "dbplyr_table_path")
  }
}

query_is_head_only <- function(x) {
  if (!inherits(x$x, "lazy_base_remote_query")) {
    return(FALSE)
  }
  if (!op_can_download(x$x)) {
    return(FALSE)
  }

  vars_base <- dbplyr::op_vars(x$x)
  if (!is_select_trivial(x$select, vars_base)) {
    return(FALSE)
  }

  if (!is_empty(x$where)) {
    return(FALSE)
  }
  if (!is_empty(x$order_by)) {
    return(FALSE)
  }
  if (!is_false(x$distinct)) {
    return(FALSE)
  }

  TRUE
}

is_select_trivial <- function(select, vars_prev) {
  identical(select$name, vars_prev) &&
    all(vapply(select$expr, is_symbol, logical(1))) &&
    identical(syms(select$name), select$expr)
}


op_rows <- function(x) UseMethod("op_rows")
#' @export
op_rows.tbl_lazy <- function(x) op_rows(x$lazy_query)
#' @export
op_rows.lazy_base_query <- function(x) Inf
#' @export
op_rows.lazy_select_query <- function(x) {
  min(x$limit, op_rows(x$x))
}

op_table <- function(x, con) UseMethod("op_table")
#' @export
op_table.tbl_lazy <- function(x, con) op_table(x$lazy_query, con)
#' @export
op_table.lazy_query <- function(x, con) NULL
#' @export
op_table.lazy_base_remote_query <- function(x, con) x$x
#' @export
op_table.lazy_select_query <- function(x, con) {
  if (!query_is_head_only(x)) {
    return(NULL)
  }

  op_table(x$x)
}
