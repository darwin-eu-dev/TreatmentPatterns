library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("CohortGenerator", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  
  global <- generateCohortTableCG()
  
  tempDir <- tempdir()

  TreatmentPatterns::executeTreatmentPatterns(
    cohorts = global$cohorts,
    cohortTableName = global$cohortTableName,
    connectionDetails = global$connectionDetails,
    cdmSchema = global$cdmSchema,
    resultSchema = global$resultSchema,
    outputPath = tempDir
  )

  expect_true(
    file.exists(file.path(tempDir, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "summaryEventDuration.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsSex.csv"))
  )
})

test_that("CDMConnector", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  tempDir <- tempdir()
  
  TreatmentPatterns::executeTreatmentPatterns(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    outputPath = tempDir
  )

  expect_true(
    file.exists(file.path(tempDir, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "summaryEventDuration.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDir, "countsSex.csv"))
  )
  
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})
