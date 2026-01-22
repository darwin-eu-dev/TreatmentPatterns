library(dplyr)
library(TreatmentPatterns)
library(testthat)

test_that("multiple cohort_tables", {
  skip_on_cran()
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("CDMConnector")
  skip_if_not_installed("CirceR")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table_target <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    1L, 5L, as.Date("2014-01-01"), as.Date("2015-01-01")
  )

  cohort_table_event <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    2L, 5L, as.Date("2014-01-10"), as.Date("2014-03-10")
  )

  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = "main",
    writeSchema = "main"
  )

  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "cohort_table_event",
    table = cohort_table_event,
    overwrite = TRUE
  )

  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "cohort_table_target",
    table = cohort_table_target,
    overwrite = TRUE
  )

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = c("cohort_table_target", "cohort_table_event"),
    cdm = cdm,
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

test_that("multiple cohort_tables", {
  skip("Eunomia [2.0.0] bug")
  skip_on_cran()
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

test_that("multiple cohort_tables andt Targets", {
  skip_on_cran()
  skip_if_not_installed("CDMConnector")
  skip_if_not_installed("DatabaseConnector")
  skip_if_not_installed("CirceR")
  skip_if_not_installed("duckdb")
  
  server <- CDMConnector::eunomiaDir()
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "duckdb",
    server = server
  )
  con <- DatabaseConnector::connect(connectionDetails)
  cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "main", writeSchema = "main")
  
  cohortSet <- CDMConnector::readCohortSet(
    path = system.file(package = "TreatmentPatterns", "exampleCohorts")
  )
  
  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = cohortSet,
    name = "cohort_table"
  )
  
  cdm$target_cohort_table <- cdm$cohort_table %>%
    dplyr::filter(.data$cohort_definition_id == 8) %>%
    dplyr::mutate(cohort_definition_id = .data$cohort_definition_id + 1) %>%
    dplyr::compute(name = "target_cohort_table", temporary = FALSE, overwrite = TRUE)
  
  cdm$target_cohort_table <- cdm$target_cohort_table %>%
    dplyr::union_all(
      cdm$target_cohort_table %>%
        dplyr::mutate(cohort_definition_id = 9)
    ) %>%
    dplyr::compute(name = "target_cohort_table", temporary = FALSE, overwrite = TRUE)
  
  cohorts <- cohortSet %>%
    # Remove 'cohort' and 'json' columns
    select(-"cohort", -"json") %>%
    mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
    rename(
      cohortId = "cohort_definition_id",
      cohortName = "cohort_name",
    ) %>%
    select("cohortId", "cohortName", "type") %>%
    dplyr::add_row(
      data.frame(
        cohortId = 9,
        cohortName = "viralsinusitis_2",
        type = "target"
      )
    )
  
  outputEnv <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = c("cohort_table", "target_cohort_table"),
    cdm = cdm
  )
  
  outCounts <- outputEnv$treatmentHistoryFinal %>%
    dplyr::group_by(.data$targetCohortId) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(.data$n)
  
  expect_identical(outCounts[1], outCounts[2])
})
