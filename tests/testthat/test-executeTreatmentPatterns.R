library(testthat)
library(TreatmentPatterns)

test_that("void", {
  skip_on_cran()
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

test_that("CohortGenerator", {
  skip_on_cran()
  skip_on_os(os = "linux")
  skip_if_not(ableToRun()$CG)

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
  skip_on_cran()
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
