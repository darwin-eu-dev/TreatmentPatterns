# Tests in this file are the only tests that should run on CRAN
# skip_on_CI()

run_on_cran <- function() {
  skip_on_bioc()
  skip_on_ci()
  skip_on_covr()
  skip_on_travis()
}

if (interactive()) {
  source("./tests/testthat/helper-ableToRun.R")
  source("./tests/testthat/helper-generateCohortTableCDMC.R")
  source("./tests/testthat/helper-generateCohortTableCG.R")
}


test_that("CRAN Tests", {
  run_on_cran()

  globals <- generateCohortTableCDMC()

  outputEnv <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cdm = globals$cdm,
    globals$cohortTableName
  )

  expect_s4_class(outputEnv, class = "Andromeda")
  expect_true("analyses" %in% names(outputEnv))
  expect_true("arguments" %in% names(outputEnv))
  expect_true("attrition" %in% names(outputEnv))
  expect_true("cdm_source_info" %in% names(outputEnv))
  expect_true("cohortTable" %in% names(outputEnv))
  expect_true("cohorts" %in% names(outputEnv))
  expect_true("currentCohorts" %in% names(outputEnv))
  expect_true("eventCohorts" %in% names(outputEnv))
  expect_true("exitCohorts" %in% names(outputEnv))
  expect_true("exitHistory" %in% names(outputEnv))
  expect_true("labels" %in% names(outputEnv))
  expect_true("metadata" %in% names(outputEnv))
  expect_true("targetCohorts" %in% names(outputEnv))
  expect_true("treatmentHistory" %in% names(outputEnv))
  expect_true("treatmentHistoryFinal" %in% names(outputEnv))

  DBI::dbDisconnect(globals$con, shutdown = TRUE)

  tpRes <- TreatmentPatterns::export(outputEnv)

  expect_true(R6::is.R6(tpRes))
  expect_true("TreatmentPatternsResults" %in% class(tpRes))

  expect_s3_class(tpRes$plotSankey(), "sankeyNetwork")
  expect_s3_class(tpRes$plotSunburst(), "sunburst")
  expect_s3_class(tpRes$plotEventDuration(), "ggplot")

  tempDir <- file.path(tempdir(), "tp")

  TreatmentPatterns::exportPatientLevel(outputEnv, outputPath = tempDir)

  expect_s3_class(read.csv(file.path(tempDir, "treatment_history.csv"), nrows = 5), "data.frame")
  expect_s3_class(read.csv(file.path(tempDir, "metadata.csv")), "data.frame")
  expect_s3_class(read.csv(file.path(tempDir, "attrition.csv")), "data.frame")
  expect_s3_class(read.csv(file.path(tempDir, "cdm_source_info.csv")), "data.frame")
})
