# Version 0.1.0.9000

* New arguments `create_disposition` and `write_disposition` to `insert_upload_job()`. (#92, @krlmlr)

* New `format_dataset()` and `format_table()`. (#81, @krlmlr)

* New `list_tabledata_iter()` that allows fetching a table in chunks of varying size. (#77, @krlmlr)

* Add support for API keys via the `BIGRQUERY_API_KEY` environment variable. (#49)
