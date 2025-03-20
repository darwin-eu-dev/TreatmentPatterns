# Libraries ----
library(testthat)
library(withr)
library(TreatmentPatterns)
library(dplyr)

# Set global vars ----
JDBC_FOLDER <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
DATABASE <- Sys.getenv("DATABASE")
RESULT_SCHEMA <- Sys.getenv("SNOWFLAKE_RESULT_SCHEMA")
CDM_SCHEMA <- Sys.getenv("SNOWFLAKE_CDM_SCHEMA")

# Install Respective JDBC ----
if (dir.exists(JDBC_FOLDER)) {
  jdbcDriverFolder <- JDBC_FOLDER
} else {
  jdbcDriverFolder <- "~/.jdbcDrivers"
  dir.create(jdbcDriverFolder, showWarnings = FALSE, recursive = TRUE)
  DatabaseConnector::downloadJdbcDrivers(DATABASE, pathToDriver = jdbcDriverFolder)
  withr::defer({
    unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
  }, envir = testthat::teardown_env())
}

CONNECTION_DETAILS <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DATABASE"),
  user = Sys.getenv("SNOWFLAKE_USER"),
  password = Sys.getenv("SNOWFLAKE_PASSWORD"),
  connectionString = Sys.getenv("SNOWFLAKE_CONNECTION_STRING"),
  pathToDriver = jdbcDriverFolder
)

# Helper function ----
generateCohortTableCG <- function(cohortTableName) {
  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()

  cohortJsonFiles <- list.files(
    system.file(package = "TreatmentPatterns", "exampleCohorts"),
    full.names = TRUE
  )

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
        stringsAsFactors = FALSE
      )
    )
  }

  cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTableName)

  CohortGenerator::createCohortTables(
    connectionDetails = CONNECTION_DETAILS,
    cohortDatabaseSchema = RESULT_SCHEMA,
    cohortTableNames = cohortTableNames
  )

  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connectionDetails = CONNECTION_DETAILS,
    cdmDatabaseSchema = CDM_SCHEMA,
    cohortDatabaseSchema = RESULT_SCHEMA,
    cohortTableNames = cohortTableNames,
    cohortDefinitionSet = cohortsToCreate,
    tempEmulationSchema = RESULT_SCHEMA
  )

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
    cohortTableName = cohortTableName,
    cohortTableNames = cohortTableNames,
    resultSchema = RESULT_SCHEMA,
    cdmSchema = CDM_SCHEMA
  ))
}

test_that("Snowflake", {
  skip_if(Sys.getenv("SNOWFLAKE_CONNECTION_STRING") == "")
  
  tryCatch({
    fail()
  }, error = function(e) {
    system("::error Forced to fail")
  })

  ## Prepare ----
  cohortTableName <- "tp_cohort_table"

  globals <- generateCohortTableCG(cohortTableName)
  withr::defer({
    # When defered drop all created tables
    CohortGenerator::dropCohortStatsTables(
      connectionDetails = CONNECTION_DETAILS,
      cohortDatabaseSchema = RESULT_SCHEMA,
      cohortTableNames = globals$cohortTableNames,
      dropCohortTable = TRUE
    )
  })

  ## new() ----
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = CONNECTION_DETAILS,
    cdmSchema = CDM_SCHEMA,
    resultSchema = RESULT_SCHEMA,
    tempEmulationSchema = RESULT_SCHEMA
  )

  ## disconnect() ----
  # When defered
  withr::defer({
    cdmInterface$disconnect()
  })

  expect_true(R6::is.R6(
    cdmInterface
  ))

  ## fetchMetadata() ----
  andromeda <- Andromeda::andromeda()

  cdmInterface$fetchMetadata(andromeda)
  
  metadata <- andromeda$metadata %>%
    collect()

  expect_in(
    c("execution_start", "package_version", "r_version", "platform"),
    names(metadata)
  )

  expect_true(is.numeric(metadata$execution_start))
  expect_identical(metadata$platform, base::version$platform)
  expect_identical(nrow(metadata), 1L)
  expect_identical(ncol(metadata), 4L)

  ## fetchCdmSource()
  andromeda <- cdmInterface$fetchCdmSource(andromeda)
  # Close when defered
  withr::defer({
    Andromeda::close(andromeda)
  })

  cdm_source <- andromeda$cdm_source_info %>%
    collect()

  expect_equal(ncol(cdm_source), 10)

  ## fetchCohortTable()
  andromeda <- cdmInterface$fetchCohortTable(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    andromeda = andromeda,
    andromedaTableName = "cohort_table",
    minEraDuration = 0
  )

  expect_true("cohort_table" %in% names(andromeda))
  expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "age", "sex", "subject_id_origin") %in% names(andromeda$cohort_table)))
})
