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

#' @title DBCInterface
#'
#' @include CDMInterface.R
#'
#' @description
#' Decorated `CDMInterface` class for `CDMConnector`
#'
#' @noRd
CDMCInterface <- R6::R6Class(
  classname = "CDMCInterface",
  inherit = CDMInterface,

  # Active ----
  active = list(
    #' @field cdm `cdm_reference`
    cdm = function() return(private$.cdm)
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @template param_cdm
    #'
    #' @return (`invisible(self)`)
    initialize = function(cdm) {
      private$.cdm <- cdm
    },

    ## Overrides
    #' @description
    #' Fetch specified cohort IDs from a specified cohort table
    #'
    #' @template param_cohorts
    #' @template param_cohortTableName
    #' @template param_andromeda
    #' @param andromedaTableName (`character(1)`)\cr
    #' Name of the table in the Andromeda object where the data will be loaded.
    #' @template param_minEraDuration
    #'
    #' @return (`andromeda`)
    fetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      targetCohortIds <- cohorts %>%
        dplyr::filter(.data$type == "target") %>%
        dplyr::select("cohortId") %>%
        dplyr::pull()
      
      n <- sapply(cohortTableName, function(tableName) {
        private$.cdm[[tableName]] %>%
          dplyr::group_by(.data$subject_id) %>% 
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::pull()
      }) |> unlist()
      
      if (length(n) == 0) {
        n <- 0
      }
      
      private$dbAppendAttrition(n, andromeda, sort(cohorts$cohortId))
      
      cohortIds <- cohorts$cohortId
      
      for (tableName in cohortTableName) {
        tbl <- private$.cdm[[tableName]] %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::mutate(
            subject_id_origin = .data$subject_id
          ) %>%
          dplyr::ungroup() %>%
          mutate(r = dplyr::row_number()) %>%
          dplyr::group_by(.data$subject_id_origin) %>%
          dplyr::mutate(
            subject_id = min(.data$r, na.rm = TRUE)
          ) %>%
          dplyr::select(-"r") %>%
          dplyr::ungroup() %>%
          dplyr::filter(.data$cohort_definition_id %in% cohortIds) %>%
          dplyr::filter(!!CDMConnector::datediff("cohort_start_date", "cohort_end_date", interval = "day") >= minEraDuration) %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::ungroup() %>%
          dplyr::inner_join(
            private$.cdm$person,
            by = dplyr::join_by(subject_id_origin == person_id)
          ) %>%
          dplyr::inner_join(
            private$.cdm$concept,
            by = dplyr::join_by(gender_concept_id == concept_id)) %>%
          dplyr::mutate(
            date_of_birth = as.Date(paste0(as.character(year_of_birth), "-01-01"))) %>%
          dplyr::mutate(
            age = !!CDMConnector::datediff("date_of_birth", "cohort_start_date", interval = "year")) %>%
          dplyr::mutate(
            subject_id_origin = as.character(subject_id_origin)
          ) %>%
          dplyr::rename(sex = "concept_name") %>%
          dplyr::mutate(
            temp_date = as.Date("1970-01-01")
          ) %>%
          dplyr::mutate(
            cohort_start_date = !!CDMConnector::datediff(start = "temp_date", end = "cohort_start_date", interval = "day"),
            cohort_end_date = !!CDMConnector::datediff(start = "temp_date", end = "cohort_end_date", interval = "day")
          ) %>%
          dplyr::select(
            "cohort_definition_id",
            "subject_id",
            "subject_id_origin",
            "cohort_start_date",
            "cohort_end_date",
            "age",
            "sex"
          )
        
        if (is.null(andromeda[[andromedaTableName]])) {
          dplyr::copy_to(dest = andromeda, df = tbl, name = andromedaTableName, overwrite = TRUE)
          # andromeda[[andromedaTableName]] <- tbl
        } else {
          andromeda$tbl_temp <- tbl
          andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
            dplyr::union_all(andromeda$tbl_temp)
          andromeda$tbl_temp <- NULL
        }
      }
      
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
    },
    
    #' @description
    #' Fetch cdm_source from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`andromeda`)
    fetchCdmSource = function(andromeda) {
      cdmSource <- private$.cdm$cdm_source %>%
        dplyr::collect()
      andromeda$cdm_source_info <- cdmSource
      return(andromeda)
    },

    #' @description
    #' Destroys instance
    #' 
    #' @return (NULL)
    disconnect = function() {
      private$.cdm <- NULL
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .cdm = NULL
  )
)