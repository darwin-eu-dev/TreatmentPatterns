library(testthat)
library(TreatmentPatterns)
library(dplyr)

if (ableToRun()$CDMC) {
  andromeda <- Andromeda::andromeda()

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  withr::defer({
    Andromeda::close(andromeda)
    DBI::dbDisconnect(con, shutdown = TRUE)
  })

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("Disease X", "Drug A", "Drug B"),
    type = c("target", "event", "event")
  )

  cohort_table <- tibble(
    cohort_definition_id = c(3, 2, 1),
    subject_id = c(1, 1, 1),
    cohort_start_date = as.Date(c("2014-10-10", "2014-11-07", "2014-10-10")),
    cohort_end_date = as.Date(c("2015-08-01", "2014-12-04", "2015-08-01"))
  )

  dplyr::copy_to(con, cohort_table, overwrite = TRUE)

  localCdm <- cdmFromCon(
    con = con,
    cdmSchema = "main",
    writeSchema = "main",
    cohortTables = "cohort_table"
  )

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(cdm = localCdm)
}

test_that("Method: new", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  expect_true(R6::is.R6(
    TreatmentPatterns:::CDMInterface$new(cdm = localCdm)
  ))
})

test_that("Method: new - empty", {
  skip_on_cran()
  expect_error(
    TreatmentPatterns:::CDMInterface$new(),
    "Could not assert if CDMConnector or DatabaseConnector is being used."
  )
})

test_that("Method: fetchMetadata", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  andromeda <- cdmInterface$fetchMetadata(andromeda)

  metadata <- andromeda$metadata %>% collect()

  expect_in(
    c("execution_start", "package_version", "r_version", "platform"),
    names(metadata)
  )

  expect_identical(metadata$r_version, base::version$version.string)
  expect_identical(metadata$platform, base::version$platform)
  expect_identical(nrow(metadata), 1L)
  expect_identical(ncol(metadata), 4L)
})

test_that("Method: fetchCohortTable", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  # Update CDM with new dummy data
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    cdm = localCdm
  )

  # Viral Sinusitis
  cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    andromeda = andromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = 5
  )

  res <- andromeda$cohortTable

  expect_identical(ncol(res), 7L)
  expect_identical(res %>% collect() %>% nrow(), 3L)

  # [!] Disabled
  # Empty
  # cdmInterface$fetchCohortTable(
  #   cohorts = data.frame(
  #     cohortId = numeric(),
  #     cohortName = character(),
  #     type = character()
  #   ),
  #   cohortTableName = "cohort_table",
  #   andromeda = andromeda,
  #   andromedaTableName = "cohortTable",
  #   minEraDuration = 5
  # )
  #
  # res <- andromeda$cohortTable
  #
  # expect_identical(ncol(res), 7L)
  # expect_identical(res %>% collect() %>% nrow(), 0L)
})
