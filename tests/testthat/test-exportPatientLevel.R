test_that("exportPatientLevel", {
  testthat::skip_if_not_installed("CDMConnector")

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

  cohorts <- cohortSet %>%
    # Remove 'cohort' and 'json' columns
    select(-"cohort", -"json") %>%
    mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
    rename(
      cohortId = "cohort_definition_id",
      cohortName = "cohort_name",
    ) %>%
    select("cohortId", "cohortName", "type")

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

  expect_equal(ncol(treatment_history), 13)
  expect_equal(nrow(treatment_history), 553)

  expect_equal(ncol(metadata), 5)
  expect_equal(nrow(metadata), 1)

  expect_equal(ncol(attrition), 5)
  expect_equal(nrow(attrition), 11)

  expect_equal(ncol(cdm_source_info), 10)
  expect_equal(nrow(cdm_source_info), 1)
})
