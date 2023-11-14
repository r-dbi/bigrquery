bq_session_create <- function(project) {
  check_string(project)

  url <- bq_path(project, jobs = "")
  body <- list(
    configuration = list(
      query = list(
        query = "SELECT 1;",
        createSession = list(
          value = unbox(TRUE)
        )
      )
    )
  )

  res <- bq_post(url, body = bq_body(body))
  res$statistics$sessionInfo$sessionId
}
