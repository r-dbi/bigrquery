library(tidyverse)
source(
  system.file("discovery-doc-ingest", "ingest-functions.R", package = "gargle")
)

x <- download_discovery_document("bigquerydatatransfer:v1")
transfer <- read_discovery_document(x)

to_snake <- function(x) {
  gsub("([A-Z])", "_\\L\\1", x, perl = TRUE)
}

schemas <- list()

schemas$TransferConfig <- transfer$schemas$TransferConfig$properties |>
  enframe() |>
  unnest_wider(value) |>
  filter(is.na(readOnly), !str_detect(description, "^Deprecated")) |>
  select(-readOnly, -additionalProperties, -enumDescriptions, -enum) |>
  mutate(type = coalesce(format, type, `$ref`), `$ref` = NULL, format = NULL)

usethis::use_data(schemas, overwrite = TRUE, internal = TRUE)
