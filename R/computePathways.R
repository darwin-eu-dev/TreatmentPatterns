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

#' computePathways
#'
#' Compute treatment patterns according to the specified parameters within
#' specified cohorts.
#'
#' @template param_cohorts
#' @template param_cohortTableName
#' @template param_cdm
#' @template param_connectionDetails
#' @template param_cdmSchema
#' @template param_resultSchema
#' @param tempEmulationSchema Schema used to emulate temp tables
#' @template param_minEraDuration
#' @template param_splitEventCohorts
#' @template param_splitTime
#' @template param_eraCollapseSize
#' @template param_combinationWindow
#' @template param_minPostCombinationDuration
#' @template param_filterTreatments
#' @template param_maxPathLength
#' @param analysisId (`character(1)`) Identifier for the TreatmentPatterns analysis.
#' @param description (`character(1)`) Description of the analysis.
#' @param concatTargets (`logical(1)`: `TRUE`) Should multiple target cohorts for the same person be concatenated or not?
#' @param startAnchor (`character(1)`: `"startDate"`) Start date anchor. One of: `"startDate"`, `"endDate"`
#' @param windowStart (`numeric(1)`: `0`) Offset for `startAnchor` in days.
#' @param endAnchor (`character(1)`: `"endDate"`) End date anchor. One of: `"startDate"`, `"endDate"`
#' @param windowEnd (`numeric(1)`: `0`) Offset for `endAnchor` in days.
#'
#' @return (`Andromeda::andromeda()`)
#' \link[Andromeda]{andromeda} object containing non-sharable patient level
#' data outcomes.
#'
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
#' if (ableToRun) {
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
#'   outputEnv <- computePathways(
#'     cohorts = cohorts,
#'     cohortTableName = "cohort_table",
#'     cdm = cdm
#'   )
#'
#'   Andromeda::close(outputEnv)
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
computePathways <- function(
    cohorts,
    cohortTableName,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    analysisId = 1,
    description = "",
    tempEmulationSchema = NULL,
    startAnchor = "startDate",
    windowStart = 0,
    endAnchor = "endDate",
    windowEnd = 0,
    minEraDuration = 0,
    splitEventCohorts = NULL,
    splitTime = NULL,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    concatTargets = TRUE) {
  validateComputePathways()

  args <- eval(
    expr = expression(mget(names(formals()))),
    envir = sys.frame(sys.nframe())
  )

  cdmInterface <- CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema,
    tempEmulationSchema = tempEmulationSchema,
    cdm = cdm
  )

  withr::defer({
    cdmInterface$disconnect()
  })

  andromeda <- Andromeda::andromeda()

  argsToSave <- as.character(jsonlite::toJSON(args[-grep("cdm|connectionDetails", names(args))]))

  andromeda$arguments <- data.frame(
    analysis_id = analysisId,
    arguments = argsToSave
  )

  andromeda$analyses <- data.frame(
    analysis_id = analysisId,
    description = description
  )

  andromeda <- cdmInterface$fetchMetadata(andromeda)
  andromeda <- cdmInterface$fetchCdmSource(andromeda)
  andromeda <- cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    andromeda = andromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = minEraDuration
  )

  checkCohortTable(andromeda)

  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::rename(
      cohortId = "cohort_definition_id",
      personId = "subject_id",
      startDate = "cohort_start_date",
      endDate = "cohort_end_date"
    )

  andromeda <- constructPathways(
    settings = args,
    andromeda = andromeda
  )

  andromeda$metadata <- andromeda$metadata %>%
    dplyr::collect() %>%
    dplyr::mutate(execution_end = as.numeric(Sys.time()))

  attrCounts <- fetchAttritionCounts(andromeda, "treatmentHistory")
  appendAttrition(
    toAdd = data.frame(
      number_records = attrCounts$nRecords,
      number_subjects = attrCounts$nSubjects,
      reason_id = 9,
      reason = sprintf("treatment construction done")
    ),
    andromeda = andromeda
  )
  return(andromeda)
}


validateComputePathways <- function() {
  args <- eval(
    # Expression to get names of function arguments in current function
    expr = expression(mget(names(formals()))),
    # Run expression in function that calls `validateComputePathways`
    envir = sys.frame(sys.nframe() - 1)
  )

  if (args$minEraDuration > args$minPostCombinationDuration) {
    warning("The `minPostCombinationDuration` is set lower than the `minEraDuration`, this might result in unexpected behavior")
  }

  if (args$minEraDuration > args$combinationWindow) {
    warning("The `combinationWindow` is set lower than the `minEraDuration`, this might result in unexpected behavior")
  }

  assertCol <- checkmate::makeAssertCollection()

  checkmate::assertNumeric(
    x = args$minEraDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "minEraDuration"
  )

  checkmate::assertIntegerish(
    x = args$splitEventCohorts,
    null.ok = TRUE,
    add = assertCol,
    .var.name = "splitEventCohorts"
  )

  checkmate::assertIntegerish(
    x = args$splitTime,
    lower = 0,
    null.ok = TRUE,
    add = assertCol,
    .var.name = "splitTime"
  )

  checkmate::assertNumeric(
    x = args$eraCollapseSize,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "eraCollapseSize"
  )

  checkmate::assertNumeric(
    x = args$combinationWindow,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "combinationWindow"
  )

  checkmate::assertNumeric(
    x = args$minPostCombinationDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "minPostCombinationDuration"
  )

  checkmate::assertCharacter(
    x = args$filterTreatments,
    len = 1,
    add = assertCol,
    .var.name = "filterTreatments"
  )

  checkmate::assertSubset(
    x = args$filterTreatments,
    choices = c("First", "Changes", "All"),
    add = assertCol,
    .var.name = "filterTreatments"
  )

  checkmate::assertNumeric(
    x = args$maxPathLength,
    lower = 0,
    upper = Inf,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "maxPathLength"
  )

  checkmate::assertDataFrame(
    x = args$cohorts,
    types = c("integerish", "character", "character"),
    any.missing = FALSE,
    all.missing = FALSE,
    ncols = 3,
    min.rows = 1,
    col.names = "named",
    add = assertCol,
    .var.name = "cohorts"
  )

  checkmate::assertSubset(
    x = names(args$cohorts),
    choices = c("cohortId", "cohortName", "type"),
    add = assertCol,
    .var.name = "cohorts"
  )

  types <- args$cohorts$type
  if (!"target" %in% types) {
    stop("No 'target' cohort specified in `cohorts`.")
  }

  checkmate::assertSubset(
    x = args$cohorts$type,
    choices = c("event", "target", "exit"),
    add = assertCol,
    .var.name = "cohorts$type"
  )

  checkmate::assertCharacter(
    x = args$cohortTableName,
    null.ok = FALSE,
    .var.name = "cohortTableName"
  )

  checkmate::assertClass(
    x = args$connectionDetails,
    classes = "ConnectionDetails",
    null.ok = TRUE,
    add = assertCol,
    .var.name = "connectionDetails"
  )

  checkmate::assertCharacter(
    x = args$connectionDetails$dbms,
    len = 1,
    null.ok = TRUE,
    add = assertCol,
    .var.name = "connectionDetails"
  )

  checkmate::assertCharacter(
    args$cdmDatabaseSchema,
    null.ok = TRUE,
    len = 1,
    add = assertCol,
    .var.name = "cdmDatabaseSchema"
  )

  checkmate::assertCharacter(
    args$resultSchema,
    null.ok = TRUE,
    len = 1,
    add = assertCol,
    .var.name = "resultSchema"
  )

  checkmate::assertClass(
    args$cdm,
    classes = "cdm_reference",
    null.ok = TRUE,
    add = assertCol,
    .var.name = "cdm"
  )

  checkmate::assertChoice(
    args$startAnchor,
    choices = c("startDate", "endDate"),
    null.ok = FALSE,
    add = assertCol,
    .var.name = "startAnchor"
  )

  checkmate::assertChoice(
    args$endAnchor,
    choices = c("startDate", "endDate"),
    null.ok = FALSE,
    add = assertCol,
    .var.name = "endAnchor"
  )

  checkmate::assertIntegerish(
    args$windowStart,
    null.ok = FALSE,
    len = 1,
    add = assertCol,
    .var.name = "windowStart"
  )

  checkmate::assertIntegerish(
    args$windowEnd,
    null.ok = FALSE,
    len = 1,
    add = assertCol,
    .var.name = "windowEnd"
  )

  checkmate::assertLogical(
    x = args$concatTargets,
    len = 1,
    add = assertCol,
    null.ok = FALSE,
    .var.name = "concatTargets"
  )

  checkmate::reportAssertions(collection = assertCol)
}

checkCohortTable <- function(andromeda) {
  cohortTableHead <- andromeda[["cohortTable"]] %>%
    head() %>%
    dplyr::collect()

  assertions <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(cohortTableHead$cohort_definition_id, add = assertions)
  checkmate::assertIntegerish(cohortTableHead$subject_id, add = assertions)
  checkmate::assertIntegerish(cohortTableHead$cohort_start_date, add = assertions)
  checkmate::assertIntegerish(cohortTableHead$cohort_end_date, add = assertions)
  checkmate::reportAssertions(assertions)
}
