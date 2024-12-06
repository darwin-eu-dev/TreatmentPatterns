library(testthat)
library(TreatmentPatterns)

test_that("void", {
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("CohortGenerator", {
  skip_if_not(ableToRun()$CG)
  skip_on_os(os = "linux")
  
  global <- generateCohortTableCG()
  
  result <- TreatmentPatterns::executeTreatmentPatterns(
    cohorts = global$cohorts,
    cohortTableName = global$cohortTableName,
    connectionDetails = global$connectionDetails,
    cdmSchema = global$cdmSchema,
    resultSchema = global$resultSchema
  )

  expect_true("TreatmentPatternsResults" %in% class(result))
})

test_that("CDMConnector", {
  skip_if_not(ableToRun()$CDMC)
  
  globals <- generateCohortTableCDMC()
  
  result <- TreatmentPatterns::executeTreatmentPatterns(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  expect_true("TreatmentPatternsResults" %in% class(result))
  
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})
