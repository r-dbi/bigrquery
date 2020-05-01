print(!identical(Sys.getenv("BIGRQUERY_PASSWORD"), ""))
print(gargle:::secret_can_decrypt("bigrquery"))


if (gargle:::secret_can_decrypt("bigrquery")) {
  json <- gargle:::secret_read("bigrquery", "bigrquery-testing.json")
  bq_auth(path = rawToChar(json))
  print(.auth$cred)
  print(class(.auth$cred))
}
