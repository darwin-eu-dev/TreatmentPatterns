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

#' @title CDMInterface
#'
#' @description
#' Decorator for an bbstract interface to the CDM, using CDMConnector or
#' DatabaseConnector.
#'
#' @noRd
CDMInterface <- R6::R6Class(
  classname = "CDMInterface",

  ## Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Fetch meta data from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`andromeda`)
    fetchMetadata = function(andromeda) {
      andromeda$metadata <- data.frame(
        execution_start = as.numeric(Sys.time()),
        package_version = as.character(utils::packageVersion("TreatmentPatterns")),
        r_version = base::version$version.string,
        platform = base::version$platform
      )
      return(andromeda)
    },

    ### Interfaces ----
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
    fetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration = NULL) {
      return(andromeda)
    },

    #' @description
    #' Fetch cdm_source from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`andromeda`)
    fetchCdmSource = function(andromeda) {
      return(andromeda)
    },

    #' @description
    #' Disconnects from database
    #' 
    #' @return `NULL`
    disconnect = function() {
      return(invisible(NULL))
    }
  ),

  ## Private ----
  private = list(
    ### Methods ----
    finalize = function() {
      self$disconnect()
    },

    dbAppendAttrition = function(n, andromeda, cohortIds) {
      appendAttrition(
        toAdd = data.frame(
          number_records = sum(n),
          number_subjects = length(n),
          reason_id = 1,
          reason = sprintf("Qualifying records for cohort definitions: %s", paste(cohortIds, collapse = ", ")),
          time_stamp = as.numeric(Sys.time())
        ),
        andromeda = andromeda
      )
    }
  )
)

makeCdmInterface <- function(connectionDetails = NULL, cdmSchema = NULL, resultSchema = NULL, tempEmulationSchema = NULL, cdm = NULL) {
  if (!is.null(cdm)) {
    CDMCInterface$new(cdm)
  } else if (!any(sapply(list(connectionDetails, cdmSchema, resultSchema), is.null))) {
    DBCInterface$new(connectionDetails, cdmSchema, resultSchema, tempEmulationSchema)
  } else {
    stop(sprintf(
      "Cannot assert to use `CDMConnector` or `DatabaseConnector`"
    ))
  }
}
