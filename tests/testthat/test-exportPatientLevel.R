test_that("exportPatientLevel", {
  skip_on_cran()
  skip_if_not_installed("CDMConnector")
  skip_if_not_installed("CirceR")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  cohortSet <- CDMConnector::readCohortSet(
    path = system.file(package = "TreatmentPatterns", "exampleCohorts")
  )

  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = cohortSet,
    name = "cohort_table"
  )

  cohorts <- cohortSet |>
    dplyr::select(
      cohortId = "cohort_definition_id",
      cohortName = "cohort_name"
    ) |>
    dplyr::mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target"))

  outputEnv <- computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm
  )

  exportPatientLevel(outputEnv, tempdir())

  treatment_history <- read.csv(file.path(tempdir(), "treatment_history.csv"))
  metadata <- read.csv(file.path(tempdir(), "metadata.csv"))
  attrition <- read.csv(file.path(tempdir(), "attrition.csv"))
  cdm_source_info <- read.csv(file.path(tempdir(), "cdm_source_info.csv"))

  testthat::expect_equal(ncol(treatment_history), 15)
  testthat::expect_equal(nrow(treatment_history), 553)

  testthat::expect_equal(ncol(metadata), 5)
  testthat::expect_equal(nrow(metadata), 1)

  testthat::expect_equal(ncol(attrition), 7)
  testthat::expect_equal(nrow(attrition), 12)

  testthat::expect_equal(ncol(cdm_source_info), 10)
  testthat::expect_equal(nrow(cdm_source_info), 1)
})
