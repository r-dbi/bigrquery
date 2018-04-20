token_path <- function() {
  path <- "~/.bigrquery-token.json"
  if (file.exists(path)) {
    return(path)
  }

  if (!requireNamespace("openssl", quietly = TRUE)) {
    return()
  }

  path <- "tests/testthat/service-token.json.enc"
  if (!file.exists(path)) {
    return()
  }

  (file_decrypt(path, tempfile()))
}

find_key <- function() {
  pass <- Sys.getenv("BIGQUERY_TOKEN_PASSWORD", "")
  if (identical(pass, "")) {
    return()
  }

  openssl::sha256(charToRaw(pass))
}

nonce <- function() {
  sodium::hex2bin("0d6da25f0d11ba9b95e5a51c04127f4d4afdde6f9a201e37")
}

file_encrypt <- function(src, dst, key = find_key()) {
  raw <- readBin(src, "raw", file.size(src))
  enc <- sodium::data_encrypt(raw, key, nonce = nonce())
  attr(enc, "nonce") <- NULL

  writeBin(enc, dst)
  invisible(dst)
}

file_decrypt <- function(src, dst, key = find_key()) {
  raw <- readBin(src, "raw", file.size(src))
  dec <- sodium::data_decrypt(raw, key, nonce = nonce())

  writeBin(dec, dst)
  invisible(dst)
}

# file_encrypt("~~/Desktop/bigrquery-examples-d37557b06951.json", "tests/testthat/service-token.json.enc")

path <- token_path()
has_auth <- file.exists(path)

skip_if_no_auth <- function() {
  if (!has_auth) {
    skip("Authentication not available")
  }
}

if (has_auth)
  set_service_token(path)
