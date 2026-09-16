testthat::test_that("tableTargetIndexToEndOfObservation", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )
  
  result <- summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1)
  
  testthat::expect_no_error({
    tableTargetIndexToEndOfObservation(result)
  })
  
  testthat::expect_no_error({
    tableTargetIndexToEndOfObservation(result, style = "darwin")
  })
  
  result <- summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1000)
  
  testthat::expect_no_error({
    tableTargetIndexToEndOfObservation(result)
  })
  
  testthat::expect_no_error({
    tableTargetIndexToEndOfObservation(result, style = "darwin")
  })
})
