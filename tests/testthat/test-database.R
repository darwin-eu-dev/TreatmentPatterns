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
  skip_if(is.null(Sys.getenv("SNOWFLAKE_SERVER")))
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = Sys.getenv("DATABASE"),
    user = Sys.getenv("SNOWFLAKE_USER"),
    password = Sys.getenv("SNOWFLAKE_PASSWORD"),
    server = Sys.getenv("SNOWFLAKE_SERVER"),
    connectionString = Sys.getenv("SNOWFLAKE_CONNECTION_STRING"),
    pathToDriver = jdbcDriverFolder
  )

  ## new() ----
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )

  expect_true(R6::is.R6(
    cdmInterface
  ))

  ## disconnect() ----
  cdmInterface$disconnect()
})
