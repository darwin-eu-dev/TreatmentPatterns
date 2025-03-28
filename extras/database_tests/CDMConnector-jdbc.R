library(CDMConnector)
library(DatabaseConnector)
library(DBI)
library(dplyr)
library(testthat)

JDBC_FOLDER <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
RESULT_SCHEMA <- Sys.getenv("RESULT_SCHEMA")
CDM_SCHEMA <- Sys.getenv("CDM_SCHEMA")

USER <- Sys.getenv("USER")
PASSWORD <- Sys.getenv("PASSWORD")

DBMS <- if (Sys.getenv("DBMS") == "") {
  NULL
} else {
  Sys.getenv("DBMS")
}
CONNECTION_STRING <- if (Sys.getenv("CONNECTION_STRING") == "") {
  NULL
} else {
  Sys.getenv("CONNECTION_STRING")
}
SERVER <- if (Sys.getenv("SERVER") == "") {
  NULL
} else {
  Sys.getenv("SERVER")
}
PORT <- if (Sys.getenv("PORT") == "") {
  NULL
} else {
  Sys.getenv("PORT")
}
EXTRA_SETTINGS <- if (Sys.getenv("EXTRA_SETTINGS") == "") {
  NULL
} else {
  Sys.getenv("EXTRA_SETTINGS")
}

test_that("Test Database", {
  skip_if(DBMS == "")
  
  # Install Respective JDBC ----
  if (dir.exists(JDBC_FOLDER)) {
    jdbcDriverFolder <- JDBC_FOLDER
  } else {
    jdbcDriverFolder <- "~/.jdbcDrivers"
    dir.create(jdbcDriverFolder, showWarnings = FALSE, recursive = TRUE)
    DatabaseConnector::downloadJdbcDrivers(DBMS, pathToDriver = jdbcDriverFolder)
    withr::defer({
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
    }, envir = testthat::teardown_env())
  }
  
  # Connection Details ----
  CONNECTION_DETAILS <- DatabaseConnector::createConnectionDetails(
    dbms = DBMS,
    user = USER,
    password = PASSWORD,
    connectionString = CONNECTION_STRING,
    server = SERVER,
    port = PORT,
    extraSettings = EXTRA_SETTINGS,
    pathToDriver = jdbcDriverFolder
  )

  con <- DatabaseConnector::connect(CONNECTION_DETAILS)

  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = CDM_SCHEMA,
    writeSchema = RESULT_SCHEMA
  )

  ## Prepare ----
  cohortTableName <- "temp_tp_cohort_table"
  
  cdm[[cohortTableName]] <- cdm$observation_period %>%
    dplyr::mutate(
      cohort_definition_id = 1,
    ) %>%
    dplyr::inner_join(
      cdm$person, dplyr::join_by(person_id == person_id)
    ) %>%
    dplyr::select(
      cohort_definition_id,
      subject_id = "person_id",
      cohort_start_date = "observation_period_start_date",
      cohort_end_date = "observation_period_start_date"
    ) %>%
    head(10) %>%
    dplyr::compute(name = cohortTableName)
  
  withr::defer({
    CDMConnector::dropSourceTable(cdm, cohortTableName)
  })

  ## new() ----
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    tempEmulationSchema = NULL,
    cdm = cdm
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
  
  andromeda <- cdmInterface$fetchMetadata(andromeda)
  
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
  
  expect_true(ncol(cdm_source) >= 10)
  
  ## fetchCohortTable()
  cohorts <- data.frame(
    cohortId = 1,
    cohortName = "foo",
    type = "target"
  )
  
  andromeda <- cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    andromeda = andromeda,
    andromedaTableName = "cohort_table",
    minEraDuration = 0
  )
  
  expect_true("cohort_table" %in% names(andromeda))
  expect_true(all(c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "age", "sex", "subject_id_origin") %in% names(andromeda$cohort_table)))
  
  cohort_table <- andromeda$cohort_table %>%
    collect()
  
  expect_true(nrow(cohort_table) == 10)
})
