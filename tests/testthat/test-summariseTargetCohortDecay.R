testthat::test_that("summariseTargetCohortDecay", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  testthat::expect_no_error(
    summariseTargetCohortDecay(outputEnv, minCellCount = 1)
  )

  testthat::expect_error(
    summariseTargetCohortDecay(outputEnv)
  )

  # cohorts
  outputEnv$backup <- outputEnv$cohorts
  outputEnv$cohorts <- NULL
  testthat::expect_error(
    summariseTargetCohortDecay(outputEnv, minCellCount = 1)
  )
  outputEnv$cohorts <- outputEnv$backup
  
  # analyses
  outputEnv$backup <- outputEnv$analyses
  outputEnv$analyses <- NULL
  testthat::expect_error(
    summariseTargetCohortDecay(outputEnv, minCellCount = 1)
  )
  outputEnv$analyses <- outputEnv$backup
  
  # cohortTable
  outputEnv$backup <- outputEnv$cohortTable
  outputEnv$cohortTable <- NULL
  testthat::expect_error(
    summariseTargetCohortDecay(outputEnv, minCellCount = 1)
  )
  outputEnv$cohortTable <- outputEnv$backup
})

testthat::test_that("plotTargetCohortDecay", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  result <- summariseTargetCohortDecay(outputEnv, minCellCount = 1)

  testthat::expect_no_error(
    plotTargetCohortDecay(result, timeScale = "day")
  )

  testthat::expect_no_error(
    plotTargetCohortDecay(result, timeScale = "week")
  )

  testthat::expect_no_error(
    plotTargetCohortDecay(result, timeScale = "month")
  )

  testthat::expect_no_error(
    plotTargetCohortDecay(result, timeScale = "year")
  )

  testthat::expect_error(
    plotTargetCohortDecay(result, timeScale = "quarter")
  )

  testthat::expect_no_error(
    plotTargetCohortDecay(result, timeScale = "year", style = "darwin")
  )
})

testthat::test_that("tableTargetCohortDecay", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )

  result <- summariseTargetCohortDecay(outputEnv, minCellCount = 1)

  testthat::expect_no_error(
    tableTargetCohortDecay(result)
  )

  testthat::expect_no_error(
    tableTargetCohortDecay(result, timeScale = "day")
  )

  testthat::expect_no_error(
    tableTargetCohortDecay(result, timeScale = "week")
  )

  testthat::expect_no_error(
    tableTargetCohortDecay(result, timeScale = "month")
  )

  testthat::expect_no_error(
    tableTargetCohortDecay(result, timeScale = "year")
  )

  testthat::expect_error(
    tableTargetCohortDecay(result, timeScale = "quarter")
  )

  testthat::expect_no_error(
    tableTargetCohortDecay(result, timeScale = "year", style = "darwin")
  )
})

testthat::test_that("tableTargetCohortDecayAtDays", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  
  outputEnv <- computePathways(
    cohorts = .CM$cohorts,
    cdm = .CM$cdm,
    .CM$cohortTableName
  )
  
  result <- summariseTargetCohortDecay(outputEnv, minCellCount = 1)

  testthat::expect_no_error(
    tableTargetCohortDecayAtDays(result, timePoints = 1)
  )

  testthat::expect_no_error(
    tableTargetCohortDecayAtDays(result, timePoints = 1, timeScale = "day")
  )

  testthat::expect_no_error(
    tableTargetCohortDecayAtDays(result, timePoints = 1, timeScale = "week")
  )

  testthat::expect_no_error(
    tableTargetCohortDecayAtDays(result, timePoints = 1, timeScale = "month")
  )

  testthat::expect_no_error(
    tableTargetCohortDecayAtDays(result, timePoints = 1, timeScale = "year")
  )

  testthat::expect_error(
    tableTargetCohortDecayAtDays(result, timePoints = 1, timeScale = "quarter")
  )

  testthat::expect_error(
    tableTargetCohortDecayAtDays(result, timePoints = -1, timeScale = "year")
  )

  testthat::expect_no_error(
    tableTargetCohortDecayAtDays(result, timePoints = 1, timeScale = "year", style = "darwin")
  )
})
