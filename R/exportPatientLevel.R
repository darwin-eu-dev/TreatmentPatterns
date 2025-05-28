addOriginId <- function(table) {
  andromeda <- table$src$con

  andromeda$cohortTable %>%
    dplyr::select("personId", "subject_id_origin") %>%
    dplyr::inner_join(table, by = dplyr::join_by(personId == personId)) %>%
    dplyr::distinct()
}

writeTreatmentHistory <- function(andromeda, outputPath) {
  writePath <- file.path(outputPath, "treatment_history.csv")
  message(sprintf("Writing treatment history to: %s", writePath))
  andromeda$treatmentHistoryFinal %>%
    addOriginId() %>%
    dplyr::select(-"sortOrder") %>%
    dplyr::collect() %>%
    dplyr::mutate(
      eventStartDate = as.Date(.data$eventStartDate),
      eventEndDate = as.Date(.data$eventEndDate)
    ) %>%
    write.csv(writePath, row.names = FALSE)
}

writeMetadata <- function(andromeda, outputPath) {
  writePath <- file.path(outputPath, "metadata.csv")
  message(sprintf("Writing metadata to: %s", writePath))
  andromeda$metadata %>%
    dplyr::collect() %>%
    write.csv(writePath, row.names = FALSE)
}

writeAttrition <- function(andromeda, outputPath) {
  writePath <- file.path(outputPath, "attrition.csv")
  message(sprintf("Writing attrition to: %s", writePath))
  andromeda$attrition %>%
    dplyr::collect() %>%
    write.csv(writePath, row.names = FALSE)
}

writeCdmSourceInfo <- function(andromeda, outputPath) {
  writePath <- file.path(outputPath, "cdm_source_info.csv")
  message(sprintf("Writing cdm source info to: %s", writePath))
  andromeda$cdm_source_info %>%
    dplyr::collect() %>%
    write.csv(writePath, row.names = FALSE)
}

#' exportPatientLevel
#'
#' Exports patient-level files for custom data analysis.
#'
#' @param andromeda (`Andromeda`) Andromeda object from `computePathways()`.
#' @param outputPath (`character(1)`) Directory where to write output files to.
#'
#' @returns `NULL`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   library(CDMConnector)
#'   library(DBI)
#'   library(TreatmentPatterns)
#'
#'   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
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
#'   exportPatientLevel(outputEnv, tempdir())
#' }
exportPatientLevel <- function(andromeda, outputPath) {
  dir.create(outputPath, showWarnings = FALSE, recursive = TRUE)
  writeTreatmentHistory(andromeda, outputPath)
  writeMetadata(andromeda, outputPath)
  writeAttrition(andromeda, outputPath)
  writeCdmSourceInfo(andromeda, outputPath)
}
