tweaks <- DBItest::tweaks(
  # The function that constructs the driver is called "dbi_driver". Using the
  # default "bigrquery" would be awkward.
  constructor_name = "dbi_driver",

  # BigQuery has very strict rules for identifiers, even if quoted.
  strict_identifier = TRUE,

  # BigQuery doesn't have a BLOB data type.
  omit_blob_tests = TRUE,

  # UNION is very special in BigQuery; needs subqueries for "inline" queries
  # and uses the comma operator instead of the UNION keyword.  Constructed by
  # this function.
  union = function(x) {
    if (length(x) == 1L) {
      x
    } else {
      #http://stackoverflow.com/a/10645603/946850
      paste(
        "SELECT * FROM (",
        paste(x, collapse = "),\n("),
        ")",
        sep = "\n")
    }
  }
)



# Make temporary dataset --------------------------------------------------

env <- new.env()
env$project <- "bigrquery-examples"
env$dataset <- paste0("test", sample(10000, 1))
insert_dataset(env$project, env$dataset)
reg.finalizer(env, function(env) {
  delete_dataset(env$project, env$dataset, deleteContents = TRUE)
}, onexit = TRUE)

DBItest::make_context(
  dbi_driver(),
  connect_args = list(
    project = env$project,
    dataset = env$dataset,
    quiet = TRUE
  ),
  tweaks = tweaks
)
