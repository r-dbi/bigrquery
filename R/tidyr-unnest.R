#' @title Unnest BigQuery record columns
#' @inheritParams tidyr::unnest
#' @export
#' @importFrom tidyr unnest
#' @examples
#' # NOT RUN
#' # check if record columns exist in dataset
#' # get_records(con, "eg_dataset.eg_table")
#' tbl(con, "eg_dataset.eg_table") |>
#'   unnest(record_column) |>
#'   head(10)

unnest.tbl_BigQueryConnection <- function(data, cols, ...,
                                          keep_empty = FALSE,
                                          ptype = NULL,
                                          names_sep = NULL,
                                          names_repair = "check_unique",
                                          .drop = deprecated(),
                                          .id = deprecated(),
                                          .sep = deprecated(),
                                          .preserve = deprecated()) {

  if(names_repair %in% c("universal", "minimal")) {
    stop('names_repair argument must be "check_unique" or "unique"')
  }

  # hard code join type
  type <- "LEFT"

  # quo nested col name
  nested_cols <- enquo(cols)
  nested_name <- quo_name(nested_cols)

  # prep sim data
  out <- data
  sim_data <- dbplyr::simulate_vars(data)
  # sim_data <- group_by(sim_data, !!!syms(group_vars(data)))
  nested_loc <- tidyselect::eval_select(expr(!!nested_name), sim_data)

  retain_names <- names(sim_data)
  names(retain_names) <- retain_names
  retain_names <- retain_names[-nested_loc]

  # get nested col's internal cols
  base_table <- unlist(out[["lazy_query"]], recursive = TRUE)[[1]]
  records <- bq_get_records(out[["src"]][["con"]],
                            base_table)[[nested_name]]

  # construct col names using name sep
  if(is.null(names_sep)) {
    cols_unnested <- records
  } else {
    cols_unnested <- paste0(nested_name,names_sep,records)
  }
  cols_nested <- paste0(nested_name,".",cols_unnested)
  names(cols_nested) <- cols_unnested

  # mimic tidyr::unnest error messages
  if(length(intersect(retain_names,cols_unnested))>0 &
     names_repair == "check_unique") {

    dups <- intersect(retain_names,cols_unnested)
    dups <- paste0(backtick(dups), " and ", backtick(nested_name),".",backtick(dups))

    bullets <- rep("*",length(dups))
    names(dups) <- rep("*",length(dups))
    abort(message = c("Names must be unique.",
                      "x" = "These names are duplicated:"),
          body = dups,
          footer = c("i"= "Use argument `names_repair` to specify repair strategy."))
  } else if(length(intersect(retain_names,cols_unnested))>0) {
    dups <- intersect(retain_names,cols_unnested)
    dup_locs <- tidyselect::eval_select(dups, cols_nested)
    names(dup_locs) <- paste0(nested_name,"___",dups)
    names(cols_nested) <- replace(names(cols_nested),dup_locs,names(dup_locs))

  }

  # replace record col with unnested cols
  new_vars <- c(retain_names[1:nested_loc-1]
                ,cols_nested,
                retain_names[nested_loc:length(retain_names)])

  # construct lazy_query
  data$lazy_query <- add_unnest(.data = out,
                                vars = syms(new_vars),
                                outer = nested_name,
                                inner = names(cols_nested),
                                retained = names(retain_names),
                                # inner = cols_unnested,
                                type = type)

  data

}

#'
#'
add_unnest <- function(.data, vars, outer, inner, retained, type) {

  lazy_query <- .data$lazy_query
  lazy_unnest_query(x = lazy_query, last_op = "unnest", select = vars,
                    outer = outer, inner = inner, retained = retained,
                    type = type)

}

#'
#'
lazy_unnest_query <- function(x, last_op, outer = outer, type = type,
                              inner = inner, retained = retained,
                              select = NULL, where = NULL,
                              group_by = NULL, order_by = NULL,
                              limit = NULL, distinct = FALSE,
                              group_vars = NULL,
                              order_vars = NULL, frame = NULL) {
  # most of this is pulled from dbplyr::select.tbl_lazy
  # more investigation needed to ensure dbplyr functions can follow unnest
  stopifnot(inherits(x, "lazy_query"))
  stopifnot(rlang::is_string(last_op))
  stopifnot(is.null(select) || dbplyr:::is_lazy_sql_part(select))
  stopifnot(dbplyr:::is_lazy_sql_part(where))
  stopifnot(dbplyr:::is_lazy_sql_part(order_by))
  stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) ==
                                 1L))
  stopifnot(is.logical(distinct), length(distinct) == 1L)
  select <- select %||% syms(set_names(op_vars(x)))
  group_vars <- group_vars %||% dbplyr::op_grps(x)
  order_vars <- order_vars %||% dbplyr::op_sort(x)
  frame <- frame %||% dbplyr:::op_frame(x)
  if (last_op == "mutate") {
    select <- dbplyr:::new_lazy_select(select, group_vars = group_vars,
                                       order_vars = order_vars, frame = frame)
  }
  else {
    select <- dbplyr:::new_lazy_select(select)
  }

  # create op info
  dbplyr:::lazy_query(query_type = "unnest", x = x, outer = outer,
                      inner = inner, retained = retained,
                      type = type, select = select, group_by = group_by,
                      order_by = order_by, distinct = distinct,
                      limit = limit, last_op = last_op,
                      group_vars = group_vars, order_vars = order_vars,
                      frame = frame)
}

