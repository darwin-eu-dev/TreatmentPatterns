library(testthat)
library(TreatmentPatterns)
library(dplyr)

test_that("Method: new", {
  skip("Eunomia [2.0.0] bug")
  skip_on_cran()
  skip_if_not(ableToRun()$CG)
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )

  expect_true(R6::is.R6(
    cdmInterface
  ))

  cdmInterface$disconnect()
})

test_that("Method: fetchMetadata", {
  skip("Eunomia [2.0.0] bug")
  skip_on_cran()
  skip_if_not(ableToRun()$CG)
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )

  andromeda <- Andromeda::andromeda()

  cdmInterface$fetchMetadata(andromeda)

  metadata <- andromeda$metadata %>% collect()

  expect_in(
    c("cdmSourceName", "cdmSourceAbbreviation", "cdmReleaseDate", "vocabularyVersion"),
    names(metadata)
  )

  expect_identical(metadata$rVersion, base::version$version.string)
  expect_identical(metadata$platform, base::version$platform)
  expect_identical(nrow(metadata), 1L)
  expect_identical(ncol(metadata), 8L)
  cdmInterface$disconnect()
})

test_that("Method: fetchCohortTable", {
  skip("Eunomia [2.0.0] bug")
  skip_on_cran()
  skip_if_not(ableToRun()$CG)
  globals <- generateCohortTableCG()

  andromeda <- Andromeda::andromeda()
  andromedaTableName <- "cohortTable"

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )

  cdmInterface$fetchCohortTable(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    andromeda = andromeda,
    andromedaTableName = andromedaTableName,
    minEraDuration = 0
  )

  cdmInterface$disconnect()

  expect_equal(names(andromeda), andromedaTableName)
})

test_that("fetchCohortTable: empty", {
  skip("Eunomia [2.0.0] bug")
  skip_on_cran()
  skip_if_not(ableToRun()$CG)
  globals <- generateCohortTableCG()

  andromeda <- Andromeda::andromeda()
  andromedaTableName <- "cohortTable"

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )

  cohorts <- data.frame(
    cohortId = numeric(),
    cohortName = character(),
    type = character()
  )

  # Empty
  cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = globals$cohortTableName,
    andromeda = andromeda,
    andromedaTableName = andromedaTableName,
    minEraDuration = 5
  )

  res <- andromeda[[andromedaTableName]] %>% dplyr::collect()

  cdmInterface$disconnect()

  expect_identical(ncol(res), 6L)
  expect_identical(nrow(res), 0L)
})

test_that("Method: disconnect", {
  skip("Eunomia [2.0.0] bug")
  skip_on_cran()
  skip_if_not(ableToRun()$CG)
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )

  andromeda <- Andromeda::andromeda()

  cdmInterface$disconnect()

  expect_error(cdmInterface$fetchMetadata(andromeda))
})
