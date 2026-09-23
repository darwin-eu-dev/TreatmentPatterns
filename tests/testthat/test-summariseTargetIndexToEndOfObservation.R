testthat::test_that("summariseTargetIndexToEndOfObservation", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv)
  )

  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = -1)
  )

  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = "1")
  )

  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = Inf)
  )

  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = NULL)
  )

  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = TRUE)
  )

  testthat::expect_no_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = NA)
  )

  testthat::expect_no_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1)
  )

  # treatmentHistoryFinal
  outputEnv$backup <- outputEnv$treatmentHistoryFinal
  outputEnv$treatmentHistoryFinal <- NULL
  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1)
  )
  outputEnv$treatmentHistoryFinal <- outputEnv$backup

  # cohorts
  outputEnv$backup <- outputEnv$cohorts
  outputEnv$cohorts <- NULL
  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1)
  )
  outputEnv$cohorts <- outputEnv$backup

  # analyses
  outputEnv$backup <- outputEnv$analyses
  outputEnv$analyses <- NULL
  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1)
  )
  outputEnv$analyses <- outputEnv$backup

  # cohortTable
  outputEnv$backup <- outputEnv$cohortTable
  outputEnv$cohortTable <- NULL
  testthat::expect_error(
    summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1)
  )
  outputEnv$cohortTable <- outputEnv$backup

  testthat::expect_true({
    result <- summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1000)
    lglN <- is.na(result$n)
    lglpct <- is.na(result$pct)

    all(
      isFALSE(lglN[1]),
      isFALSE(lglpct[1]),
      isTRUE(lglN[2]),
      isTRUE(lglpct[2])
    )
  })
})

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

testthat::test_that("plotTargetIndexToEndOfObservation", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )
  
  result <- summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1)

  testthat::expect_no_error({
    plotTargetIndexToEndOfObservation(result)
  })
  
  testthat::expect_no_error({
    plotTargetIndexToEndOfObservation(result, style = "darwin")
  })
  
  result <- summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 1000)
  
  testthat::expect_no_error({
    plotTargetIndexToEndOfObservation(result)
  })
  
  testthat::expect_no_error({
    plotTargetIndexToEndOfObservation(result, style = "darwin")
  })
})
