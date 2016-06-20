path <- "service-token.json"
has_auth <- file.exists(path)

if (has_auth)
  set_service_token(path)
