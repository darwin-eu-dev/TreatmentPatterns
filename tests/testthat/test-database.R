# Libraries ----
library(testthat)
library(withr)
library(TreatmentPatterns)
library(dplyr)

# Install Respective JDBC ----
if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- "~/.jdbcDrivers"
  dir.create(jdbcDriverFolder, showWarnings = FALSE, recursive = TRUE)
  DatabaseConnector::downloadJdbcDrivers(Sys.getenv("DATABASE"), pathToDriver = jdbcDriverFolder)
  withr::defer({
    unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
  }, envir = testthat::teardown_env())
}

# Helper function ----
generateCohortTableCG <- function(connectionDetails, cohortTableName) {
  resultSchema <- Sys.getenv("SNOWFLAKE_CDM_SCHEMA")
  cdmSchema <- Sys.getenv("SNOWFLAKE_CDM_SCHEMA")

  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()

  cohortJsonFiles <- list.files(
    system.file(
      package = "TreatmentPatterns",
      "exampleCohorts"),
    full.names = TRUE)

  for (i in seq_len(length(cohortJsonFiles))) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(
      cohortJsonFileName)$size)
    
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    
    cohortSql <- CirceR::buildCohortQuery(
      cohortExpression,
      options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortsToCreate <- rbind(
      cohortsToCreate,
      data.frame(
        cohortId = i,
        cohortName = cohortName,
        sql = cohortSql,
        stringsAsFactors = FALSE))
  }

  cohortTableNames <- CohortGenerator::getCohortTableNames(
    cohortTable = cohortTableName)

  CohortGenerator::createCohortTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = resultSchema,
    cohortTableNames = cohortTableNames)

  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmSchema,
    cohortDatabaseSchema = resultSchema,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate)

  # Select Viral Sinusitis Cohort
  targetCohorts <- cohortsGenerated %>%
    dplyr::filter(cohortName == "ViralSinusitis") %>%
    dplyr::select(cohortId, cohortName)

  # Select everything BUT Viral Sinusitis cohorts
  eventCohorts <- cohortsGenerated %>%
    dplyr::filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
    dplyr::select(cohortId, cohortName)

  exitCohorts <- cohortsGenerated %>%
    dplyr::filter(cohortName == "Death") %>%
    dplyr::select(cohortId, cohortName)
  
  cohorts <- dplyr::bind_rows(
    targetCohorts %>% dplyr::mutate(type = "target"),
    eventCohorts %>% dplyr::mutate(type = "event"),
    exitCohorts %>% dplyr::mutate(type = "exit")
  )

  return(list(
    cohorts = cohorts,
    connectionDetails = connectionDetails,
    cohortTableName = cohortTableName,
    cohortTableNames = cohortTableNames,
    resultSchema = resultSchema,
    cdmSchema = cdmSchema
  ))
}

test_that("Snowflake", {
  skip_if(Sys.getenv("SNOWFLAKE_CONNECTION_STRING") == "")

  ## Prepare ----
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = Sys.getenv("DATABASE"),
    user = Sys.getenv("SNOWFLAKE_USER"),
    password = Sys.getenv("SNOWFLAKE_PASSWORD"),
    connectionString = Sys.getenv("SNOWFLAKE_CONNECTION_STRING"),
    pathToDriver = jdbcDriverFolder
  )

  cohortTableName <- "tp_cohort_table"

  generateCohortTableCG(connectionDetails, cohortTableName)

  ## new() ----
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = Sys.getenv("SNOWFLAKE_CDM_SCHEMA"),
    resultSchema = Sys.getenv("SNOWFLAKE_RESULT_SCHEMA")
  )

  expect_true(R6::is.R6(
    cdmInterface
  ))

  ## fetchMetadata() ----
  andromeda <- Andromeda::andromeda()

  cdmInterface$fetchMetadata(andromeda)
  
  metadata <- andromeda$metadata %>%
    collect()

  expect_in(
    c("cdmSourceName", "cdmSourceAbbreviation", "cdmReleaseDate", "vocabularyVersion"),
    names(metadata)
  )

  expect_identical(metadata$rVersion, base::version$version.string)
  expect_identical(metadata$platform, base::version$platform)
  expect_identical(nrow(metadata), 1L)
  expect_identical(ncol(metadata), 8L)

  cdmInterface$fetchCohortTable(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    andromeda = andromeda,
    andromedaTableName = andromedaTableName,
    minEraDuration = 0
  )

  expect_true(andromedaTableName %in% names(andromeda))

  ## Clean up ----
  CohortGenerator::dropCohortStatsTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = Sys.getenv("SNOWFLAKE_RESULT_SCHEMA"),
    cohortTableNames = ,
    dropCohortTable = TRUE
  )

  ## disconnect() ----
  cdmInterface$disconnect()
})
