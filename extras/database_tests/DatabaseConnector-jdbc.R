# Libraries ----
library(testthat)
library(withr)
library(TreatmentPatterns)
library(dplyr)

# Set global vars ----
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

  connection <- DatabaseConnector::connect(CONNECTION_DETAILS)

  # Make CDM Reference with JDBC ----
  ## Prepare ----
  cohortTableName <- "temp_tp_cohort_table_2"

  dummyCohortTable <- data.frame(
    cohort_definition_id = 1,
    subject_id = c(1, 2, 3, 4, 5),
    cohort_start_date = as.Date("1999-01-01"),
    cohort_end_date = as.Date("2005-01-01")
  )

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = RESULT_SCHEMA,
    tableName = cohortTableName,
    data = dummyCohortTable
  )

  withr::defer({
    DatabaseConnector::renderTranslateExecuteSql(
      connection = connection,
      sql = "DORP TABLE @schema.@table",
      schema = RESULT_SCHEMA,
      table = cohortTableName
    )
  })

  cohorts <- data.frame(
    cohortId = 1,
    cohortName = "foo",
    type = "target"
  )

  andromeda <- TreatmentPatterns:::fetchCohortTable(
    connectionDetails = CONNECTION_DETAILS,
    connection = NULL,
    cdmSchema = CDM_SCHEMA,
    writeSchema = RESULT_SCHEMA,
    cohorts = cohorts,
    cohortTables = cohortTableName
  )

  withr::defer({
    Andromeda::close(andromeda)
  })

  andromeda$cohortTable <- andromeda$cohort_table |>
    dplyr::mutate(
      cohort_start_date = .data$cohort_start_date - as.Date("1970-01-01"),
      cohort_end_date = .data$cohort_end_date - as.Date("1970-01-01")
    )
  
  andromeda$cohort_table <- NULL

  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::rename(
      cohortId = "cohort_definition_id",
      personId = "subject_id",
      startDate = "cohort_start_date",
      endDate = "cohort_end_date"
    )

  testthat::expect_true(!is.null(andromeda$cohortTable))

  colCheck <- all(
    colnames(andromeda$cohortTable) %in% c(
      "cohortId", "personId", "startDate", "endDate", "sex", "cohort_name",
      "type", "observation_period_start_date", "observation_period_end_date",
      "subject_id_origin", "age")
  )

  testthat::expect_true(colCheck)

  testthat::expect_true(!is.null(andromeda$cdm_source_info))
})
