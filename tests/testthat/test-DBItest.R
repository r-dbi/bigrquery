if (has_auth) {

# bigrquery requires the following DBItest tweaks, commented inline:
tweaks <- DBItest::tweaks(
  # The function that constructs the driver is called "dbi_driver". Using the
  # default "bigrquery" would be awkward.
  constructor_name = "dbi_driver",

  # BigQuery has very strict rules for identifiers, even if quoted.
  strict_identifier = TRUE,

  # BigQuery doesn't have a BLOB data type.
  omit_blob_tests = TRUE,

  # The SQL functions current_date(), current_time() and current_timestamp()
  # need parentheses.
  current_needs_paren = TRUE,

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

# In addition, specific tests are omitted, also commented inline. If the comment
# refers to an issue number, the problem should be resolved by fixing the
# corresponding issue.  (Skipping the logical_int tests should probably be
# a tweak instead (rstats-db/DBItest#53), but this is the way it's handled for
# now.)

DBItest::make_context(
  dbi_driver(), list(project = "bigrquery-examples", dataset = "test"),
  tweaks = tweaks
)

withr::with_options(
  list(bigrquery.quiet = TRUE,
       bigrquery.page.size = 6),
  {

    res <- DBItest::test_getting_started(c(
      "package_name" # Won't change package name for this
    ))

    res <- DBItest::test_driver(c(
      NULL
    ))

    res <- DBItest::test_connection(c(
      NULL
    ))

    res <- DBItest::test_result(c(
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

    res <- DBItest::test_sql(c(
      "^temporary_table$", # Temporary tables not supported
      "^roundtrip_logical_int$", # Not an error: Full support for logical
      "^roundtrip_64_bit$", # #94
      "^roundtrip_null$", # #97
      "^roundtrip_date$", # No distinction between date, time, and timestamp data types
      "^roundtrip_timestamp$", # #98
      NULL
    ))

    res <- DBItest::test_meta(c(
      "get_info_result", # rstats-db/DBI#55
      "rows_affected", # Command queries not supported
      "^bind_.*", # Later
      NULL
    ))

    res <- DBItest::test_compliance(c(
      "read_only", # No read_only mode
      NULL
    ))

  }
) # withr::with_options(

} # if (!identical(Sys.getenv("TRAVIS"), "true")) {
