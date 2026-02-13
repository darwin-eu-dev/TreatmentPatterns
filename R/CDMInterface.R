# Copyright 2024 DARWIN EU®
#
# This file is part of TreatmentPatterns
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

fetchMetadata <- function(andromeda) {
  andromeda$metadata <- data.frame(
    execution_start = as.numeric(Sys.time()),
    package_version = as.character(utils::packageVersion("TreatmentPatterns")),
    r_version = base::version$version.string,
    platform = base::version$platform
  )
  return(andromeda)
}

dbAppendAttrition <- function(n, andromeda, cohortIds) {
  appendAttrition(
    toAdd = data.frame(
      number_records = as.integer(sum(n)),
      number_subjects = as.integer(length(n)),
      reason_id = 1,
      reason = sprintf("Qualifying records for cohort definitions: %s", paste(cohortIds, collapse = ", ")),
      time_stamp = as.numeric(Sys.time())
    ),
    andromeda = andromeda
  )
}

fetchCohortTable <- function(cdm, cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
  targetCohortIds <- cohorts %>%
    dplyr::filter(.data$type == "target") %>%
    dplyr::select("cohortId") %>%
    dplyr::pull()

  n <- lapply(cohortTableName, function(tableName) {
    cdm[[tableName]] %>%
      dplyr::group_by(.data$subject_id) %>% 
      dplyr::summarise(n = as.integer(dplyr::n())) %>%
      dplyr::pull()
  }) |> unlist()
  
  if (length(n) == 0) {
    n <- 0
  }
  
  dbAppendAttrition(n, andromeda, sort(cohorts$cohortId))
  
  cohortIds <- cohorts$cohortId
  
  for (tableName in cohortTableName) {
    cdm$tp_temp_tbl <- cdm[[tableName]] %>%
      dplyr::group_by(.data$subject_id) %>%
      dplyr::mutate(
        subject_id_origin = .data$subject_id
      ) %>%
      dplyr::filter(.data$cohort_definition_id %in% cohortIds) %>%
      dplyr::filter(!!CDMConnector::datediff("cohort_start_date", "cohort_end_date", interval = "day") >= minEraDuration) %>%
      dplyr::group_by(.data$subject_id) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(
        cdm$person,
        by = dplyr::join_by(subject_id_origin == person_id)
      ) %>%
      dplyr::inner_join(
        cdm$concept,
        by = dplyr::join_by(gender_concept_id == concept_id)) %>%
      dplyr::mutate(
        date_of_birth = as.Date(paste0(as.character(.data$year_of_birth), "-01-01"))) %>%
      dplyr::mutate(
        age = !!CDMConnector::datediff("date_of_birth", "cohort_start_date", interval = "year")) %>%
      dplyr::mutate(
        subject_id_origin = as.character(.data$subject_id_origin)
      ) %>%
      dplyr::rename(sex = "concept_name") %>%
      dplyr::mutate(
        temp_date = as.Date("1970-01-01")
      ) %>%
      dplyr::mutate(
        cohort_start_date = as.integer(!!CDMConnector::datediff(start = "temp_date", end = "cohort_start_date", interval = "day")),
        cohort_end_date = as.integer(!!CDMConnector::datediff(start = "temp_date", end = "cohort_end_date", interval = "day"))
      ) %>%
      dplyr::select(
        "cohort_definition_id",
        "subject_id",
        "subject_id_origin",
        "cohort_start_date",
        "cohort_end_date",
        "age",
        "sex"
      ) %>%
      dplyr::compute(name = "tp_temp_tbl", overwrite = TRUE, temporary = FALSE)

    if (is.null(andromeda[[andromedaTableName]])) {
      dplyr::copy_to(dest = andromeda, df = cdm$tp_temp_tbl, name = andromedaTableName, overwrite = TRUE)
    } else {
      dplyr::copy_to(dest = andromeda, df = cdm$tp_temp_tbl, name = "tbl_temp", overwrite = TRUE)
      andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
        dplyr::union_all(andromeda$tbl_temp)
      andromeda$tbl_temp <- NULL
    }
  }

  if ("tp_temp_tbl" %in% CDMConnector::listSourceTables(cdm)) {
    cdm <- CDMConnector::dropSourceTable(cdm = cdm, name = "tp_temp_tbl")
  }

  andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
    dplyr::mutate(r = dplyr::row_number()) %>%
    dplyr::group_by(.data$subject_id_origin) %>%
    dplyr::mutate(
      subject_id = as.integer(min(.data$r, na.rm = TRUE))
    ) %>%
    dplyr::select(-"r")

  targetId <- as.numeric(targetCohortIds)
  
  andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
    dplyr::mutate(cohort_definition_id = as.numeric(.data$cohort_definition_id)) %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::filter(any(.data$cohort_definition_id %in% targetId, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  n <- andromeda[[andromedaTableName]] %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull()
  
  appendAttrition(
    toAdd = data.frame(
      number_records = sum(n),
      number_subjects = length(n),
      reason_id = 2,
      reason = sprintf("Removing records < minEraDuration (%s)", minEraDuration),
      time_stamp = as.numeric(Sys.time())
    ),
    andromeda = andromeda
  )
  return(andromeda)
}

fetchCdmSource = function(cdm, andromeda) {
  cdmSource <- cdm$cdm_source %>%
    dplyr::collect()
  andromeda$cdm_source_info <- cdmSource
  return(andromeda)
}
