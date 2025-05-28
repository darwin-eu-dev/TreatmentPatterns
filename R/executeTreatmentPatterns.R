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

#' executeTreatmentPatterns
#'
#' Compute treatment patterns according to the specified parameters within
#' specified cohorts. For more customization, or investigation of patient level
#' outcomes, you can run \link[TreatmentPatterns]{computePathways} and
#' \link[TreatmentPatterns]{export} separately.
#'
#' @template param_cohorts
#' @template param_cohortTableName
#' @template param_cdm
#' @template param_connectionDetails
#' @template param_cdmSchema
#' @template param_resultSchema
#' @param tempEmulationSchema (`character(1)`) Schema to emulate temp tables.
#' @template param_minEraDuration
#' @template param_eraCollapseSize
#' @template param_combinationWindow
#' @template param_minCellCount
#'
#' @return `TreatmentPatternsResults`
#' @export
#'
#' @examples
#' \donttest{
#' ableToRun <- all(
#'   require("CirceR", character.only = TRUE, quietly = TRUE),
#'   require("CDMConnector", character.only = TRUE, quietly = TRUE),
#'   require("TreatmentPatterns", character.only = TRUE, quietly = TRUE),
#'   require("dplyr", character.only = TRUE, quietly = TRUE)
#' )
#'
#' if (require("CirceR", character.only = TRUE, quietly = TRUE)) {
#'   library(TreatmentPatterns)
#'   library(CDMConnector)
#'   library(dplyr)
#'
#'   withr::local_envvar(
#'     R_USER_CACHE_DIR = tempfile(),
#'     EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
#'   )
#'
#'   tryCatch(
#'     {
#'       if (Sys.getenv("skip_eunomia_download_test") != "TRUE") {
#'         CDMConnector::downloadEunomiaData(overwrite = TRUE)
#'       }
#'     },
#'     error = function(e) NA
#'   )
#'
#'   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
#'   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main")
#'
#'   cohortSet <- readCohortSet(
#'     path = system.file(package = "TreatmentPatterns", "exampleCohorts")
#'   )
#'
#'   cdm <- generateCohortSet(
#'     cdm = cdm,
#'     cohortSet = cohortSet,
#'     name = "cohort_table"
#'   )
#'
#'   cohorts <- cohortSet %>%
#'     # Remove 'cohort' and 'json' columns
#'     select(-"cohort", -"json") %>%
#'     mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
#'     rename(
#'       cohortId = "cohort_definition_id",
#'       cohortName = "cohort_name",
#'     ) %>%
#'     select("cohortId", "cohortName", "type")
#'
#'   executeTreatmentPatterns(
#'     cohorts = cohorts,
#'     cohortTableName = "cohort_table",
#'     cdm = cdm
#'   )
#'
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
executeTreatmentPatterns <- function(
    cohorts,
    cohortTableName,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    tempEmulationSchema = NULL,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minCellCount = 5) {
  checkmate::assert_integerish(minCellCount, len = 1, null.ok = FALSE, lower = 0)

  # Compute pathways on patient level
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    cdm = cdm,
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema,
    tempEmulationSchema = tempEmulationSchema,
    minEraDuration = minEraDuration,
    splitEventCohorts = NULL,
    splitTime = NULL,
    eraCollapseSize = eraCollapseSize,
    combinationWindow = combinationWindow,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  withr::defer({
    tryCatch(
      {
        Andromeda::close(andromeda)
      },
      error = function(e) {
        message("Andromeda object was close pre-maturely")
      },
      warning = function(w) {
        message("Andromeda object was close pre-maturely")
      }
    )
  })

  TreatmentPatterns::export(
    andromeda = andromeda,
    ageWindow = 5,
    minCellCount = minCellCount,
    censorType = "mean",
    nonePaths = TRUE,
    stratify = TRUE
  )
}
