if (secret_can_decrypt("bigrquery")) {
  json <- secret_read("bigrquery", "service-token.json")
  set_service_token(rawToChar(json))
}
