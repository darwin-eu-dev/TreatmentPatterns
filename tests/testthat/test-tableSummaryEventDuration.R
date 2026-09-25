testthat::test_that("tableSummaryEventDuration", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  result <- TreatmentPatterns::export(outputEnv)

  testthat::expect_no_error(
    tableSummaryEventDuration(result)
  )

  testthat::expect_no_error(
    tableSummaryEventDuration(result, style = "darwin")
  )

  result$summary_event_duration <- NULL
  testthat::expect_error(
    tableSummaryEventDuration(result)
  )
})
