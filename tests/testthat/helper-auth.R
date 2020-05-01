if (gargle:::secret_can_decrypt("bigrquery")) {
  json <- gargle:::secret_read("bigrquery", "bigrquery-testing.json")
  bq_auth(path = rawToChar(json))
}
