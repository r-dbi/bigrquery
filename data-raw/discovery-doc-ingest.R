library(tidyverse)

source(
  system.file("discovery-doc-ingest", "ingest-functions.R", package = "gargle")
)

x <- download_discovery_document("bigquery:v2")
dd <- read_discovery_document(x)

methods <- get_raw_methods(dd)

methods <- methods %>% map(groom_properties,  dd)
methods <- methods %>% map(add_schema_params, dd)
methods <- methods %>% map(add_global_params, dd)

.endpoints <- methods
attr(.endpoints, "base_url") <- dd$rootUrl
## View(.endpoints)

usethis::use_data(.endpoints, internal = TRUE, overwrite = TRUE)
