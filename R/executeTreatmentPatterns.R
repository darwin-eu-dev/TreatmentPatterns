#' executeTreatmentPatterns
#'
#' Compute treatment patterns according to the specified parameters within
#' specified cohorts. For more customization, or investigation of patient level
#' outcomes, you can run \link[TreatmentPatterns]{computePathways} and
#' \link[TreatmentPatterns]{export} separately.
#'
#' @template param_cohorts
#' @template param_cohortTableName
#' @template param_outputPath
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
#' @return (`invisible(NULL)`)
#' @export
#'
#' @examples
#' \donttest{
#' library(TreatmentPatterns)
#' library(CDMConnector)
#' library(dplyr)
#' if (require("CirceR", character.only = TRUE, quietly = TRUE)) {
#'   withr::local_envvar(
#'     R_USER_CACHE_DIR = tempfile(),
#'     EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
#'   )
#'
#'   tryCatch({
#'     if (Sys.getenv("skip_eunomia_download_test") != "TRUE") {
#'       CDMConnector::downloadEunomiaData(overwrite = TRUE)
#'     }
#'   },
#'   error = function(e) NA)
#'
#'   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
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
#'     cdm = cdm,
#'     outputPath = tempdir()
#'   )
#'     
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
executeTreatmentPatterns <- function(
    cohorts,
    cohortTableName,
    outputPath,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    tempEmulationSchema = NULL,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minCellCount = 5) {
  checkmate::assert_character(outputPath, len = 1, null.ok = FALSE)
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
    includeTreatments = "startDate",
    indexDateOffset = 0,
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
    tryCatch({
      Andromeda::close(andromeda)
    }, error = function(e) {
      message("Andromeda object was close pre-maturely")
    }, warning = function(w) {
      message("Andromeda object was close pre-maturely")
    })
  })

  # Export csv-files
  TreatmentPatterns::export(
    andromeda = andromeda,
    outputPath = outputPath,
    ageWindow = 5,
    minCellCount = minCellCount,
    censorType = "mean",
    archiveName = "TreatmentPatterns-Output.zip"
  )
  return(invisible(NULL))
}
