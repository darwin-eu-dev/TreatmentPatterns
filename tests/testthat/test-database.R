library(testthat)
library(withr)

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

test_that("Snowflake", {
  skip_if(Sys.getenv("SNOWFLAKE_CONNECTION_STRING") == "")
  print(sprintf(">> %s", Sys.getenv("DATABASE")))

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = Sys.getenv("DATABASE"),
    user = Sys.getenv("SNOWFLAKE_USER"),
    password = Sys.getenv("SNOWFLAKE_PASSWORD"),
    connectionString = Sys.getenv("SNOWFLAKE_CONNECTION_STRING"),
    pathToDriver = jdbcDriverFolder
  )

  ## new() ----
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = Sys.getenv("SNOWFLAKE_CDM_SCHEMA"),
    resultSchema = Sys.getenv("SNOWFLAKE_RESULT_SCHEMA")
  )

  expect_true(R6::is.R6(
    cdmInterface
  ))

  ## disconnect() ----
  cdmInterface$disconnect()
})
