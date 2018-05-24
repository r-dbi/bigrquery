# Motivation:
# Make it as easy as possible to have a secret that can be access locally, on
# travis, and on r-hub, but not on anywhere else.
#
# Approach:
# * Use symmetric encryption with a password stored in an envar.
#   (use secret_pw_gen() to generate a random password)
#
# * Store encrypted files in a common location, `inst/secret`
#   (use secret_write() to create)
#
# Will fail gracefully if password not found or suggested sodium package
# not installed. Up to the user to handle absense of encryption - see
# helper-auth.R for an example
#
# Can test on r-hub by passing along env var:
# rhub::check(env_vars = Sys.getenv(secret_pw_name("bigrquery"), names = TRUE))

secret_can_decrypt <- function(package) {
  requireNamespace("sodium", quietly = TRUE) && secret_pw_exists(package)
}

# Returns a raw vector
secret_read <- function(package, name) {
  if (!secret_can_decrypt(package)) {
    stop("Decryption not available", call. = FALSE)
  }

  path <- secret_path(package, name)
  raw <- readBin(path, "raw", file.size(path))

  sodium::data_decrypt(raw, key = secret_pw_get(package), nonce = secret_nonce())
}

secret_write <- function(package, name, data) {
  if (inherits(data, "connection")) {
    data <- readBin(data, "raw", file.size(data))
  } else if (is.character(data)) {
    data <- charToRaw(data)
  }

  secret <- file.path("inst", "secret")
  if (!file.exists(secret)) {
    dir.create(secret, showWarnings = FALSE, recursive = TRUE)
  }
  dst <- file.path(secret, name)

  enc <- sodium::data_encrypt(data,
    key = secret_pw_get(package),
    nonce = secret_nonce()
  )
  attr(enc, "nonce") <- NULL
  writeBin(enc, dst)

  invisible(dst)
}

secret_path <- function(package, name) {
  stopifnot(is.character(name), length(name) == 1)

  path <- system.file("secret", name, package = package)
  if (path == "") {
    stop("Encrypted file '", name, "' not found in ", package, " package", call. = FALSE)
  }

  path
}

# Generated with sodium::bin2hex(sodium::random(24)). AFAICT nonces are
# primarily used to prevent replay attacks, which shouldn't be a concern here
secret_nonce <- function() {
  sodium::hex2bin("0d6da25f0d11ba9b95e5a51c04127f4d4afdde6f9a201e37")
}

# Password env var --------------------------------------------------------
# Locally: set in ~/.Renviron
# Travis: set in web UI

secret_pw_gen <- function() {
  x <- sample(c(letters, LETTERS, 0:9), 50, replace = TRUE)
  paste0(x, collapse = "")
}

secret_pw_name <- function(package) {
  paste0(toupper(package), "_PASSWORD")
}

secret_pw_exists <- function(package) {
  pass <- Sys.getenv(secret_pw_name(package), "")
  !identical(pass, "")
}

secret_pw_get <- function(package) {
  env <- secret_pw_name(package)
  pass <- Sys.getenv(env, "")
  if (identical(pass, "")) {
    stop("Envvar '", env, "' not defined", call. = FALSE)
  }

  sodium::sha256(charToRaw(pass))
}

