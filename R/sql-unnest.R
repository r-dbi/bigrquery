#' @importFrom dbplyr sql_build
#' @export
sql_build.lazy_unnest_query <- function(op, con = NULL, ...) {
  if (!is.null(op$message_summarise)) {
    inform(op$message_summarise)
  }
  # op$select$expr <- syms(gsub("[.]","`.`",op$select$expr))
  select_sql_list <- get_unnest_sql(op$select, "unnest",
                                    dbplyr::op_vars(op$x), con)
  where_sql <- dbplyr:::translate_sql_(op$where, con = con, context = list(clause = "WHERE"))
  unnest_query(from = dbplyr:::sql_build(op$x, con),
               unnest = sql(paste0(op$type, " JOIN UNNEST(",op$outer,") ", op$outer)),
               select = select_sql_list$select_sql, where = where_sql,
               group_by = dbplyr:::translate_sql_(op$group_by,con = con),
               window = select_sql_list$window_sql,
               order_by = dbplyr:::translate_sql_(op$order_by, con = con),
               distinct = op$distinct, limit = op$limit)
}

unnest_query <- function(from, unnest, select = sql("*"), where = character(), group_by = character(),
                         having = character(), window = character(), order_by = character(),
                         limit = NULL, distinct = FALSE) {
  stopifnot(is.character(select))
  stopifnot(is.character(unnest))
  stopifnot(is.character(where))
  stopifnot(is.character(group_by))
  stopifnot(is.character(having))
  stopifnot(is.character(window))
  stopifnot(is.character(order_by))
  stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) ==
                                 1L))
  stopifnot(is.logical(distinct), length(distinct) == 1L)
  structure(list(from = from, unnest = unnest, select = select, where = where,
                 group_by = group_by, having = having, window = window,
                 order_by = order_by, distinct = distinct, limit = limit),
            class = c("unnest_query", "query"))

}

#' @importFrom dbplyr sql_optimise
#' @export
sql_optimise.lazy_unnest_query <- function(x, con = NULL, ...,
                                           subquery = FALSE) {

}

#' @importFrom dbplyr sql_render
#' @export
sql_render.unnest_query <- function(query, con = NULL, ...,
                                    subquery = FALSE, lvl = 0,
                                    cte = FALSE) {
  from <- dbplyr:::dbplyr_sql_subquery(con,
                                       dbplyr:::sql_render(query$from, con,
                                                           ..., subquery = TRUE,
                                                           lvl = lvl + 1),
                                       name = NULL, lvl = lvl)
  split_subtick <- unlist(strsplit(from,"`"))
  sub_name <- paste0("`",split_subtick[[length(split_subtick)]],"`")

  split_col <- lapply(strsplit(query$select,"[.]"),rev)
  add_sub <- lapply(split_col, append, values = sub_name)
  subset_cols <- lapply(add_sub, function(x) paste0(rev(x[1:2]),collapse = "."))
  select_v <- unlist(subset_cols)
  query$select <- sql(select_v)

  join_unnest <- sql(paste(from,query$unnest))
  dbplyr:::dbplyr_query_select(con, query$select, from = join_unnest,
                               where = query$where,
                               group_by = query$group_by,
                               having = query$having, window = query$window,
                               order_by = query$order_by, limit = query$limit,
                               distinct = query$distinct, ...,
                               subquery = subquery, lvl = lvl)
}

get_unnest_sql <- function (select, select_operation, in_vars, con)
{
  if (select_operation == "summarise") {
    select_expr <- set_names(select$expr, select$name)
    select_sql_list <- translate_sql_(select_expr, con, window = FALSE,
                                      context = list(clause = "SELECT"))
    select_sql <- sql_vector(select_sql_list, parens = FALSE,
                             collapse = NULL, con = con)
    return(list(select_sql = select_sql, window_sql = character()))
  }
  if (dbplyr:::is_select_trivial(select, in_vars)) {
    return(list(select_sql = sql("*"), window_sql = character()))
  }
  select <- dbplyr:::select_use_star(select, in_vars, con)
  dbplyr:::win_register_activate()
  on.exit(dbplyr:::win_reset(), add = TRUE)
  on.exit(dbplyr:::win_register_deactivate(), add = TRUE)
  select_sql <- translate_unnest_sql(con, select)
  dbplyr:::win_register_deactivate()
  named_windows <- dbplyr:::win_register_names()
  if (nrow(named_windows) > 0 && dbplyr:::supports_window_clause(con)) {
    select_sql <- translate_select_sql(con, select)
    names_esc <- sql_escape_ident(con, named_windows$name)
    window_sql <- sql(paste0(names_esc, " AS ", named_windows$key))
  }
  else {
    window_sql <- character()
  }
  list(select_sql = select_sql, window_sql = window_sql)
}

translate_unnest_sql <- function (con, select_df) {

  select_df <- transmute(select_df, dots = set_names(expr, name),
                         vars_group = .data$group_vars,
                         vars_order = .data$order_vars,
                         vars_frame = .data$frame)

  out <- purrr::pmap(select_df, function(dots, vars_group,
                                         vars_order, vars_frame) {
    dbplyr:::translate_sql_(list(dots), con,
                            vars_group = dbplyr:::translate_sql_(syms(vars_group), con),
                            vars_order = dbplyr:::translate_sql_(vars_order, con,
                                                                 context = list(clause = "SELECT")),
                            vars_frame = vars_frame[[1]],
                            context = list(clause = "SELECT"))
  })
  out <- lapply(out,gsub,pattern="[.]", replacement = "`.`")
  sql(unlist(out))
}
