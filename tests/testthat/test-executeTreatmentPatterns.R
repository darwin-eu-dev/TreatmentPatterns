library(testthat)
library(TreatmentPatterns)

test_that("void", {
  skip_on_cran()
  expect_error(TreatmentPatterns::executeTreatmentPatterns())
})

# test_that("CohortGenerator", {
#   skip_on_cran()
#   skip_on_os(os = "linux")
#   skip_if_not(ableToRun()$CG)
# 
#   result <- TreatmentPatterns::executeTreatmentPatterns(
#     cohorts = .CG$cohorts,
#     cohortTableName = .CG$cohortTableName,
#     connectionDetails = .CG$connectionDetails,
#     cdmSchema = .CG$cdmSchema,
#     resultSchema = .CG$resultSchema
#   )
# 
#   expect_true("TreatmentPatternsResults" %in% class(result))
# })

test_that("CDMConnector", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  result <- TreatmentPatterns::executeTreatmentPatterns(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm
  )

  expect_true("TreatmentPatternsResults" %in% class(result))
})
