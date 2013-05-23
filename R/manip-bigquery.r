#' Data manipulation for SQL data sources.
#'
#' Arrange, filter and select are lazy: they modify the object representing
#' the table, and do not recompute unless needed.  Summarise and mutate
#' are eager: they will always return a source_df.
#'
#' @param .data an bigquery data base
#' @param ... variables interpreted in the context of \code{.data}
#' @param .n maximum number of columns to return. Set to \code{-1} to return
#'  all.
#' @examples
#' db_path <- system.file("db", "baseball.bigquery3", package = "dplyr")
#' baseball_s <- source_bigquery(db_path, "baseball")
#'
#' # filter, select and arrange lazily modify the specification of the table
#' # they don't execute queries unless you print them
#' filter(baseball_s, year > 2005, g > 130)
#' select(baseball_s, id:team)
#' arrange(baseball_s, id, desc(year))
#'
#' # summarise and mutate always return data frame sources
#' summarise(baseball_s, g = mean(g), n = count())
#' mutate(baseball_s, rbi = 1.0 * r / ab)
#'
#' @name manip_bigquery
NULL
