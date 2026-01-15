# Package index

## DBI and dplyr

- [`src_bigquery()`](https://bigrquery.r-dbi.org/dev/reference/src_bigquery.md)
  : A BigQuery data source for dplyr.
- [`dbConnect(`*`<BigQueryDriver>`*`)`](https://bigrquery.r-dbi.org/dev/reference/bigquery.md)
  : BigQuery DBI driver
- [`collect.tbl_BigQueryConnection()`](https://bigrquery.r-dbi.org/dev/reference/collect.tbl_BigQueryConnection.md)
  : Collect a BigQuery table

## Low-level API

- [`bq_dataset_create()`](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md)
  [`bq_dataset_meta()`](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md)
  [`bq_dataset_exists()`](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md)
  [`bq_dataset_update()`](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md)
  [`bq_dataset_delete()`](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md)
  [`bq_dataset_tables()`](https://bigrquery.r-dbi.org/dev/reference/api-dataset.md)
  : BigQuery datasets
- [`bq_job_meta()`](https://bigrquery.r-dbi.org/dev/reference/api-job.md)
  [`bq_job_status()`](https://bigrquery.r-dbi.org/dev/reference/api-job.md)
  [`bq_job_show_statistics()`](https://bigrquery.r-dbi.org/dev/reference/api-job.md)
  [`bq_job_wait()`](https://bigrquery.r-dbi.org/dev/reference/api-job.md)
  : BigQuery job: retrieve metadata
- [`bq_project_datasets()`](https://bigrquery.r-dbi.org/dev/reference/api-project.md)
  [`bq_project_jobs()`](https://bigrquery.r-dbi.org/dev/reference/api-project.md)
  : BigQuery project methods
- [`bq_table_create()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_meta()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_fields()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_size()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_nrow()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_exists()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_delete()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_copy()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_upload()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_save()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_load()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  [`bq_table_patch()`](https://bigrquery.r-dbi.org/dev/reference/api-table.md)
  : BigQuery tables
- [`bq_auth()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth.md) :
  Authorize bigrquery
- [`bq_auth_configure()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md)
  [`bq_oauth_client()`](https://bigrquery.r-dbi.org/dev/reference/bq_auth_configure.md)
  : Edit and view auth configuration
- [`bq_deauth()`](https://bigrquery.r-dbi.org/dev/reference/bq_deauth.md)
  : Clear current token
- [`bq_field()`](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  [`bq_fields()`](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  [`as_bq_field()`](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  [`as_bq_fields()`](https://bigrquery.r-dbi.org/dev/reference/bq_field.md)
  : BigQuery field (and fields) class
- [`bq_has_token()`](https://bigrquery.r-dbi.org/dev/reference/bq_has_token.md)
  : Is there a token on hand?
- [`bq_projects()`](https://bigrquery.r-dbi.org/dev/reference/bq_projects.md)
  : List available projects
- [`bq_project_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)
  [`bq_dataset_query()`](https://bigrquery.r-dbi.org/dev/reference/bq_query.md)
  : Submit query to BigQuery
- [`bq_dataset()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  [`as_bq_dataset()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  [`bq_table()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  [`as_bq_table()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  [`bq_job()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  [`as_bq_job()`](https://bigrquery.r-dbi.org/dev/reference/bq_refs.md)
  : S3 classes for BigQuery datasets, tables and jobs
- [`bq_table_download()`](https://bigrquery.r-dbi.org/dev/reference/bq_table_download.md)
  : Download table data
- [`bq_token()`](https://bigrquery.r-dbi.org/dev/reference/bq_token.md)
  : Produce configured token
- [`bq_user()`](https://bigrquery.r-dbi.org/dev/reference/bq_user.md) :
  Get info on current user
