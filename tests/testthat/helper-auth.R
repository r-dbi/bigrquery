has_auth <- secret_can_decrypt("bigrquery")
if (has_auth) {
  json <- secret_read("bigrquery", "service-token.json")
  set_service_token(rawToChar(json))
}

skip_if_no_auth <- function() {
  if (!has_auth) {
    skip("Authentication not available")
  }
}

