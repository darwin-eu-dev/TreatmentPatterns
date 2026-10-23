testthat::test_that("summariseGaps", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  testthat::expect_no_error(
    summariseGaps(outputEnv)
  )

  # treatmentHistoryFinal
  outputEnv$backup <- outputEnv$treatmentHistoryFinal
  outputEnv$treatmentHistoryFinal <- NULL
  testthat::expect_error(
    summariseGaps(outputEnv)
  )
  outputEnv$treatmentHistoryFinal <- outputEnv$backup
  
  # cohorts
  outputEnv$backup <- outputEnv$cohorts
  outputEnv$cohorts <- NULL
  testthat::expect_error(
    summariseGaps(outputEnv)
  )
  outputEnv$cohorts <- outputEnv$backup
  
  # analyses
  outputEnv$backup <- outputEnv$analyses
  outputEnv$analyses <- NULL
  testthat::expect_error(
    summariseGaps(outputEnv)
  )
  outputEnv$analyses <- outputEnv$backup
})

testthat::test_that("plotGaps", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  result <- summariseGaps(outputEnv)

  testthat::expect_no_error(
    tableGaps(result)
  )

  testthat::expect_no_error(
    tableGaps(result, timeScale = "day")
  )

  testthat::expect_no_error(
    tableGaps(result, timeScale = "week")
  )

  testthat::expect_no_error(
    tableGaps(result, timeScale = "month")
  )

  testthat::expect_no_error(
    tableGaps(result, timeScale = "year")
  )

  testthat::expect_error(
    tableGaps(result, timeScale = "foo")
  )

  testthat::expect_no_error(
    tableGaps(result, style = "darwin")
  )
})

testthat::test_that("tableGaps", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )
  
  result <- summariseGaps(outputEnv)

  testthat::expect_no_error(
    plotGaps(result)
  )
  
  testthat::expect_no_error(
    plotGaps(result, timeScale = "day")
  )
  
  testthat::expect_no_error(
    plotGaps(result, timeScale = "week")
  )
  
  testthat::expect_no_error(
    plotGaps(result, timeScale = "month")
  )
  
  testthat::expect_no_error(
    plotGaps(result, timeScale = "year")
  )
  
  testthat::expect_error(
    plotGaps(result, timeScale = "foo")
  )
  
  testthat::expect_no_error(
    plotGaps(result, style = "darwin")
  )
})
