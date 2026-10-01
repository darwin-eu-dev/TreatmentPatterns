testthat::test_that("summariseOnevent", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )
  
  testthat::expect_error(
    sumamriseOnEvent(outputEnv)
  )

  testthat::expect_no_error(
    summariseOnEvent(outputEnv, minCellCount = 5)
  )

  testthat::expect_no_error(
    summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter", timeGroup = 90)
  )

  testthat::expect_warning(
    summariseOnEvent(outputEnv, minCellCount = 5, timeGroup = 90)
  )

  testthat::expect_error(
    summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter")
  )

  resultDay <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "day")
  resultWeek <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "week")
  resultMonth <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "month")
  resultQuarter <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter", timeGroup = 90)
  resultYear <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "year")

  testthat::expect_true(
    nrow(resultDay) > nrow(resultWeek)
  )

  testthat::expect_true(
    nrow(resultWeek) > nrow(resultMonth)
  )

  testthat::expect_true(
    nrow(resultMonth) > nrow(resultQuarter)
  )

  testthat::expect_true(
    nrow(resultQuarter) > nrow(resultYear)
  )

  # cohortTable
  outputEnv$backup <- outputEnv$cohortTable
  outputEnv$cohortTable <- NULL
  testthat::expect_error(
    summariseOnEvent(outputEnv)
  )
  outputEnv$cohortTable <- outputEnv$backup

  # treatmentHistoryFinal
  outputEnv$backup <- outputEnv$treatmentHistoryFinal
  outputEnv$treatmentHistoryFinal <- NULL
  testthat::expect_error(
    summariseOnEvent(outputEnv)
  )
  outputEnv$treatmentHistoryFinal <- outputEnv$backup
  
  # cohorts
  outputEnv$backup <- outputEnv$cohorts
  outputEnv$cohorts <- NULL
  testthat::expect_error(
    summariseOnEvent(outputEnv)
  )
  outputEnv$cohorts <- outputEnv$backup
  
  # analyses
  outputEnv$backup <- outputEnv$analyses
  outputEnv$analyses <- NULL
  testthat::expect_error(
    summariseOnEvent(outputEnv)
  )
  outputEnv$analyses <- outputEnv$backup
})

testthat::test_that("tableOnEvent", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )
  resultQuarter <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter", timeGroup = 90)
  resultYear <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "year")

  testthat::expect_no_error(
    tableOnEvent(resultYear)
  )

  testthat::expect_no_error(
    tableOnEvent(resultQuarter)
  )
})

testthat::test_that("plotOnEvent", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )
  
  resultQuarter <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter", timeGroup = 90)
  resultYear <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "year")
  
  testthat::expect_no_error(
    plotOnEvent(resultYear)
  )

  testthat::expect_no_error(
    plotOnEvent(resultQuarter)
  )
})
