#* Get latest version required proto from googleapis
system("git clone --depth 1 https://github.com/googleapis/googleapis ./scripts/googleapis")

fpath <- grep("bigquery/storage/v1/", dir("./scripts/googleapis", "storage.proto$", full.names = TRUE, recursive = TRUE), value = TRUE)

import_path <- function(fpath) {
  imports <- lapply(fpath, function(path) { grep("^import", readLines(path), value = TRUE) })
  imports <- gsub("import \"|\";", "", sort(unique(unlist(imports))))
  if (length(imports) > 0) {
    imports <- paste0("./scripts/googleapis/", imports)
    imports <- imports[file.exists(imports)]
    return(sort(unique(c(imports, import_path(imports)))))
  }
  return()
}

fpath <- c(fpath, import_path(fpath))
inst_path <- gsub(".*scripts/googleapis/", "./inst/protos/", fpath)
for (path in inst_path) {
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
}
file.copy(fpath, inst_path, overwrite = TRUE)
