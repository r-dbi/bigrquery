if (secret_can_decrypt("bigrquery")) {
  json <- secret_read("bigrquery", "service-token.json")
  bq_auth(path = rawToChar(json))
}
