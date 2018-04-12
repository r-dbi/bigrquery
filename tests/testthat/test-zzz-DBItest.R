if (FALSE) {

  tweaks <- DBItest::tweaks(
    constructor_name = "bigquery",

    # BigQuery has very strict rules for identifiers, even if quoted.
    strict_identifier = TRUE,

    # BigQuery doesn't have a BLOB data type.
    omit_blob_tests = TRUE,
    # Or a time type
    time_typed = FALSE,

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
    bigquery(),
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
    "package_name",
    NULL
  ))

  DBItest::test_driver(c(
    NULL
  ))

  DBItest::test_connection(c(
    "^disconnect_invalid_connection$", # DBItest::with_invalid_connection doesn't generate invalid BigQuery connection
    NULL
  ))

  DBItest::test_result(c(
    "^.*invalid_connection$", # see above
    "^data_type_connection$", # dbDataType() not implemented, no DDL in BigQuery
    "^data_64_bit.*", # #94
    "^data_date.*", # Requires modification in DBItest due to nonstandard syntax
    "^data_time.*", # Requires modification in DBItest due to nonstandard syntax,

    "^fetch_no_return_value$", # can't create in traditional sql
    "^clear_result_return_statement$",
    "^cannot_clear_result_twice_statement$",
    "^send_statement_trivial$",
    "^send_statement_result_valid$",
    "^send_statement_only_one_result_set$",
    "^execute_atomic$",
    "^data_type_create_table$",
    "^data_character$", # Error: Cannot union tables : Incompatible types. 'a' : BOOL 'a' : STRING
    NULL
  ))

  DBItest::test_sql(c(
    "^temporary_table$", # Temporary tables not supported
    "^roundtrip_null$", # #97
    "^roundtrip_date$", # No distinction between date, time, and timestamp data types
    "^roundtrip_timestamp$", # #98
    "^roundtrip_integer$", # https://github.com/r-dbi/DBItest/issues/164
    "^roundtrip_numeric$",
    "^roundtrip_logical$",
    "^roundtrip_character.*$",
    "^roundtrip_64_bit.*$", # #94
    "^unquote_identifier.*$", # no support for unquoting yet,
    "^read_table_.*$", # https://github.com/r-dbi/DBItest/issues/168
    "^write_table_.*$",
    "^overwrite_table.*$",
    "^append_table.*$",
    "^table_visible_in_other_connection$",
    "^roundtrip_factor$",
    "^roundtrip_quotes$",
    "^roundtrip_mixed$",
    "^roundtrip_keywords$",
    "^roundtrip_field_types$",
    "^remove_table.*$",
    "^list_fields.*$",
    "^list_tables$",
    "^exists_table.*$",
    "^.*invalid_connection$", # see above
    "^list_objects.*$", # no schema support
    NULL
  ))

  DBItest::test_meta(c(
    "get_info_result", # rstats-db/DBI#55
    "rows_affected", # Command queries not supported
    "^bind_.*", # Later
    "^is_valid_.*$",
    "^.*_statement$",
    "^get_rows_affected_error$",
    NULL
  ))

  DBItest::test_compliance(c(
    "read_only", # No read_only mode
    "reexport",
    NULL
  ))

}
