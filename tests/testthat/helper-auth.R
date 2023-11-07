if (gargle::secret_has_key("BIGRQUERY_KEY")) {
  path <- system.file("secret", "bigrquery-testing.json", package = "bigrquery")
  json <- gargle::secret_decrypt_json(path, "BIGRQUERY_KEY")
  bq_auth(path = json)
}
