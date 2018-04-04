if (has_auth) {

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

  test_dataset <- setup({
    x <- paste0("test", sample(10000, 1))
    insert_dataset(bq_test_project(), x)
    x
  })
  teardown({delete_dataset(bq_test_project(), test_dataset, deleteContents = TRUE)})

  DBItest::make_context(
    dbi_driver(),
    connect_args = list(
      project = bq_test_project(),
      dataset = test_dataset,
      quiet = TRUE
    ),
    tweaks = tweaks
  )

  # In addition, specific tests are omitted, also commented inline. If the comment
  # refers to an issue number, the problem should be resolved by fixing the
  # corresponding issue.  (Skipping the logical_int tests should probably be
  # a tweak instead (rstats-db/DBItest#53), but this is the way it's handled for
  # now.)

  DBItest::test_getting_started(c(
    "package_name" # Won't change package name for this
  ))

  DBItest::test_driver(c(
    NULL
  ))

  DBItest::test_connection(c(
    "^disconnect_invalid_connection$", # DBItest::with_invalid_connection doesn't generate invalid BigQuery connection
    NULL
  ))

  DBItest::test_result(c(
    "^command_query$", # Command queries not supported
    "^fetch_no_return_value$", # Command queries not supported
    "^data_logical_int.*", # No error: Support for logical values available
    "^data_character_null_(above|below)$", # Error: Cannot union tables : Incompatible types. 'a' : TYPE_STRING 'a' : TYPE_BOOL
    "^data_type_connection$", # dbDataType() not implemented, no DDL in BigQuery
    "^data_64_bit.*", # #94
    "^data_date.*", # Requires modification in DBItest due to nonstandard syntax
    "^data_time.*", # Requires modification in DBItest due to nonstandard syntax
    NULL
  ))

  DBItest::test_sql(c(
    "^temporary_table$", # Temporary tables not supported
    "^roundtrip_logical_int$", # Not an error: Full support for logical
    "^roundtrip_64_bit$", # #94
    "^roundtrip_null$", # #97
    "^roundtrip_date$", # No distinction between date, time, and timestamp data types
    "^roundtrip_timestamp$", # #98
    "^roundtrip_integer$", # https://github.com/r-dbi/DBItest/issues/164
    "^roundtrip_numeric$",
    "^roundtrip_logical$",
    "^roundtrip_character$",
    "^roundtrip_numeric_special$", # no way to send literal Inf/-Inf
    "^quote_identifier_not_vectorized$", # bug in CRAN version
    NULL
  ))

  DBItest::test_meta(c(
    "get_info_result", # rstats-db/DBI#55
    "rows_affected", # Command queries not supported
    "^bind_.*", # Later
    NULL
  ))

  DBItest::test_compliance(c(
    "read_only", # No read_only mode
    NULL
  ))

}
