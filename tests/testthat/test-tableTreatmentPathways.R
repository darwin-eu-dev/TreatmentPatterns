testthat::test_that("tableTreatmentPathways", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  result <- TreatmentPatterns::export(outputEnv)

  testthat::expect_no_error(
    tableTreatmentPathways(result)
  )

  testthat::expect_no_error(
    tableTreatmentPathways(result, style = "darwin")
  )

  result$treatment_pathways <- NULL
  testthat::expect_error(
    tableTreatmentPathways(result)
  )
})
