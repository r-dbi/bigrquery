#' Manage TransferConfigs
#'
#' @param data_source_id `r rd_schema_param("TransferConfig", "dataSourceId")`
#' @param destination_dataset_id `r rd_schema_param("TransferConfig", "destinationDatasetId")`
#' @param schedule `r rd_schema_param("TransferConfig", "schedule")`
#' @param display_name `r rd_schema_param("TransferConfig", "displayName")`
#' @param params `r rd_schema_param("TransferConfig", "params")`
#' @param schedule_options `r rd_schema_param("TransferConfig", "scheduleOptions")`
#' @param data_refresh_window_days `r rd_schema_param("TransferConfig", "dataRefreshWindowDays")`
#' @param disabled `r rd_schema_param("TransferConfig", "disabled")`
#' @param ... Additional arguments are automatically converted to camelCamel
#'   and added to the TransferConfig object.
bq_transfer_config_create <- function(project_id,
                                      name,
                                      data_source_id,
                                      destination_dataset_id,
                                      location_id = NULL,
                                      schedule = "",
                                      display_name = NULL,
                                      params = list(),
                                      schedule_options = list(),
                                      data_refresh_window_days = NULL,
                                      disabled = FALSE,
                                      ...
                                      ) {

  path <- bq_transfer_config_path(project_id, location_id)

  body <- bq_transfer_config(
    name = name,
    data_source_id = data_source_id,
    destination_dataset_id = destination_dataset_id,
    schedule = schedule,
    display_name = display_name,
    params = params,
    schedule_options = schedule_options,
    data_refresh_window_days = data_refresh_window_days,
    disabled = disabled,
    ...
  )

  bq_post(path, body)
}

bq_transfer_config_patch <- function(project_id,
                                     config_id,
                                     location_id = NULL,
                                     name = NULL,
                                     data_source_id = NULL,
                                     destination_dataset_id = NULL,
                                     schedule = NULL,
                                     display_name = NULL,
                                     params = NULL,
                                     schedule_options = NULL,
                                     data_refresh_window_days = NULL,
                                     disabled = NULL,
                                     ...
                                     ) {

  path <- bq_transfer_config_path(project_id, location_id, config_id)

  body <- bq_transfer_config(
    name = name,
    data_source_id = data_source_id,
    destination_dataset_id = destination_dataset_id,
    schedule = schedule,
    display_name = display_name,
    params = params,
    schedule_options = schedule_options,
    data_refresh_window_days = data_refresh_window_days,
    disabled = disabled,
    ...,
    .patch = TRUE
  )

  bq_patch(path, body)
}

bq_transfer_config_get <- function(project_id,
                                   config_id,
                                   location_id = NULL) {
  path <- bq_transfer_config_path(project_id, location_id, config_id)
  bq_get(path)
}

bq_transfer_config_list <- function(project_id,
                                    location_id = NULL) {
  path <- bq_transfer_config_path(project_id, location_id)
  bq_get(path)
}

bq_transfer_config_delete <- function(project_id,
                                      config_id,
                                      location_id = NULL) {
  path <- bq_transfer_config_path(project_id, location_id, config_id)
  bq_delete(path)
}

bq_transfer_config_path <- function(project_id, location_id = NULL, config_id = NULL, ...) {
  check_string(project_id)
  check_string(config_id, allow_null = TRUE)
  check_string(location_id, allow_null = TRUE)

  components <- c(
    project = project_id,
    location = location_id,
    transferConfig = config_id,
    ...
  )

  paste0(names(components), "/", components, collapse = "/")
}


# https://cloud.google.com/bigquery/docs/reference/datatransfer/rest/v1/projects.locations.transferConfigs#TransferConfig
bq_transfer_config <- function(name,
                               data_source_id,
                               destination_dataset_id,
                               schedule = "",
                               display_name = NULL,
                               params = list(),
                               schedule_options = list(),
                               data_refresh_window_days = NULL,
                               disabled = FALSE,
                               ...,
                               .patch = FALSE
                               ) {

  check_string(data_source_id, allow_empty = FALSE, allow_null = .patch)
  check_string(destination_dataset_id, allow_empty = FALSE, allow_null = .patch)
  check_string(schedule, allow_null = .patch)
  check_string(display_name, allow_null = TRUE, allow_null = .patch)
  check_number(data_refresh_window_days, allow_null = TRUE, allow_null = .patch)
  check_bool(disabled, allow_null = .patch)

  camelList(
    name = name,
    dataSourceId = data_source_id,
    destinationDatasetId = destination_dataset_id,
    displayName = display_name,
    params = params,
    schedule = schedule,
    scheduleOptions = schedule_options,
    dataRefreshWindowDays = data_refresh_window_days,
    disabled = disabled,
    ...
  )
}


rd_schema_param <- function(schema_name, param_name) {
  schema <- schemas[[schema_name]]
  desc <- schema$description[schema$name == toCamel(param_name)]
  gsub("(https://[^ ]+[^. ?])", "<\\1>", desc)
}
