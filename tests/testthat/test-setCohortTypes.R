testthat::test_that("CohortSet", {
  testthat::skip_on_cran()
  testthat::skip_if_not(ableToRun()$CDMC)

  cohortSet <- CDMConnector::readCohortSet(
    path = system.file(package = "TreatmentPatterns", "exampleCohorts")
  )

  testthat::expect_error(
    setCohortTypes(cohortSet)
  )

  testthat::expect_error(
    setCohortTypes(cohortSet, "event")
  )

  testthat::expect_no_error(
    setCohortTypes(cohortSet, c(rep("event", 7), "target"))
  )
})

testthat::test_that("cohortsToCreate", {
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  
  cohortJsonFiles <- list.files(path = system.file("exampleCohorts", package = "TreatmentPatterns"), full.names = TRUE)
  
  for (i in 1:length(cohortJsonFiles)) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <- data.frame(
      cohortId = i,
      cohortName = cohortName, 
      sql = cohortSql,
      stringsAsFactors = FALSE
    ) |>
      rbind(cohortsToCreate)
  }

  testthat::expect_error(
    setCohortTypes(cohortsToCreate)
  )
  
  testthat::expect_error(
    setCohortTypes(cohortsToCreate, "event")
  )
  
  testthat::expect_no_error(
    setCohortTypes(cohortsToCreate, c(rep("event", 7), "target"))
  )
})

testthat::test_that("CohortsGenerated", {
  testthat::skip_on_cran()
  testthat::skip_if_not(ableToRun()$CG)

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
  
  cohortJsonFiles <- list.files(path = system.file("exampleCohorts", package = "TreatmentPatterns"), full.names = TRUE)
  
  for (i in 1:length(cohortJsonFiles)) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <- data.frame(
      cohortId = i,
      cohortName = cohortName, 
      sql = cohortSql,
      stringsAsFactors = FALSE
    ) |>
      rbind(cohortsToCreate)
  }
  
  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = "my_cohort_table")
  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = "main",
    cohortTableNames = cohortTableNames
  )
  
  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    cohortDatabaseSchema = "main",
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate
  )

  # cohortsGenerated
  testthat::expect_error(
    setCohortTypes(cohortsGenerated)
  )
  
  testthat::expect_error(
    setCohortTypes(cohortsGenerated, "event")
  )
  
  testthat::expect_no_error(
    setCohortTypes(cohortsGenerated, c(rep("event", 7), "target"))
  )
})