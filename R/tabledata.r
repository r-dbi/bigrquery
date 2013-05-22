
list_tabledata <- function(project, dataset, table, max_pages = Inf) {
  cat("Retrieving data")
  table_info <- get_table(project, dataset, table)
  schema <- table_info$schema

  url <- sprintf("projects/%s/datasets/%s/tables/%s/data", project, dataset,
    table)
  cur_page <- 0

  elapsed <- timer()
  data <- bq_get(url)
  rows <- list(extract_data(data$rows, schema))

  while(cur_page < max_pages && !is.null(data$rows)) {
    cat("\rRetrieving data: ", sprintf("%4.1f", elapsed()), "s", sep = "")
    data <- bq_get(url, query = list(pageToken = data$pageToken, maxResults = 2048))
    rows <- c(rows, list(extract_data(data$rows, schema)))
  }
  cat("\n")

  do.call("rbind", rows)
}


converter <- list(
  integer = as.integer,
  float = as.double,
  boolean = as.logical,
  string = identity
)

extract_data <- function(rows, schema) {
  if (is.null(rows)) return(NULL)

  types <- tolower(vapply(schema$fields, function(x) x$type, character(1)))

  # Convert NULLs into NAs
  out <- character(length(rows) * length(types))
  for(i in seq_along(rows)) {
    for(j in seq_along(types)) {
      if (is.null(rows[[i]]$f[[j]]$v)) rows[[i]]$f[[j]]$v <- NA_character_
    }
  }
  data <- unlist(rows, use.names = FALSE)
  data_m <- matrix(data, nrow = length(types))

  out <- vector("list", length(types))
  for(i in seq_along(types)) {
    type <- types[[i]]
    if (!(type %in% names(converter))) {
      stop("Don't know how to convert type ", type, call. = FALSE)
    }
    out[[i]] <- converter[[type]](data_m[i, ])
  }

  names(out) <- vapply(schema$fields, function(x) x$name, character(1))
  as_df(out)
}
