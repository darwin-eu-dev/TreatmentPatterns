library(dplyr)
library(TreatmentPatterns)
library(testthat)

test_that("multiple cohort_tables", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table_target <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01")
  )

  cohort_table_event <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10")
  )

  copy_to(con, cohort_table_target, overwrite = TRUE)
  copy_to(con, cohort_table_event, overwrite = TRUE)
  
  cdm <- cdmFromCon(
    con = con,
    cdmSchema = "main",
    writeSchema = "main",
    cohortTables = c("cohort_table_target", "cohort_table_event")
  )

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = c("cohort_table_target", "cohort_table_event"),
    cdm = cdm,
    includeTreatments = "startDate",
    indexDateOffset = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
  
  expect_identical(result$treatment_pathways$pathway, "A")
  
  DBI::dbDisconnect(con)
})

test_that("multiple cohort_tables", {
  skip_if_not(ableToRun()$CG)
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table_target <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01")
  )

  cohort_table_event <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10")
  )

  conDet <- Eunomia::getEunomiaConnectionDetails()
  con <- DatabaseConnector::connect(conDet)
  
  DatabaseConnector::dbWriteTable(conn = con, name = "cohort_table_target", value = cohort_table_target)
  DatabaseConnector::dbWriteTable(conn = con, name = "cohort_table_event", value = cohort_table_event)

  DatabaseConnector::disconnect(con)

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = c("cohort_table_target", "cohort_table_event"),
    connectionDetails = conDet,
    cdmSchema = "main",
    resultSchema = "main",
    tempEmulationSchema = NULL,
    includeTreatments = "startDate",
    indexDateOffset = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
  
  expect_identical(result$treatment_pathways$pathway, "A")
})
