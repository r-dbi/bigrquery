path <- "~/.bigrquery-token.json"
has_auth <- file.exists(path)

skip_if_no_auth <- function() {
  if (!has_auth) {
    skip("Authentication not available")
  }
}

if (has_auth)
  set_service_token(path)
