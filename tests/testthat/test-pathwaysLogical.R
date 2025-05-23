library(testthat)
library(TreatmentPatterns)
library(dplyr)

test_that("A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
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

  DBI::dbDisconnect(con)
})

test_that("A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})


test_that("A-B-C", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    4,                     5,           as.Date("2014-05-14"), as.Date("2014-07-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-C")

  DBI::dbDisconnect(con)
})

test_that("A-B+C", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     1,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     1,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     1,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    4,                     1,           as.Date("2014-03-10"), as.Date("2014-05-10")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B+C")

  DBI::dbDisconnect(con)
})

test_that("A+B-C", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    4,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A+B-C")

  DBI::dbDisconnect(con)
})

test_that("A-A+B", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     1,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     1,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    2,                     1,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     1,           as.Date("2014-03-10"), as.Date("2014-05-10")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A+B")

  DBI::dbDisconnect(con)
})

test_that("A-B-A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     5,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    2,                     5,           as.Date("2014-05-12"), as.Date("2014-07-12"),
    3,                     5,           as.Date("2014-07-14"), as.Date("2014-09-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-A-B")

  DBI::dbDisconnect(con)
})

test_that("A-B-A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     3,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     3,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     3,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    2,                     3,           as.Date("2014-05-12"), as.Date("2014-07-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-A")

  DBI::dbDisconnect(con)
})

test_that("A-B-B, collapse to A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     7,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     7,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     7,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     7,           as.Date("2014-05-12"), as.Date("2014-06-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})

test_that("A-B-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     7,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     7,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     7,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     7,           as.Date("2014-06-12"), as.Date("2014-07-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-B")

  DBI::dbDisconnect(con)
})

test_that("A+B-A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     5,           as.Date("2014-03-20"), as.Date("2014-05-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A+B-A")

  DBI::dbDisconnect(con)
})

test_that("A-A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     5,           as.Date("2014-04-12"), as.Date("2014-06-12"),
    3,                     5,           as.Date("2014-06-14"), as.Date("2014-08-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A-B")

  DBI::dbDisconnect(con)
})

test_that("A-A-B, collapse to A-B (Changes)", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    3,                     5,           as.Date("2014-05-14"), as.Date("2014-06-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "Changes",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})

test_that("A-A-B, collapse to A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    3,                     5,           as.Date("2014-05-14"), as.Date("2014-06-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})

test_that("A+B-A+B, collapse to A+B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    3,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A+B")

  DBI::dbDisconnect(con)
})

test_that("A+B-A+B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     5,           as.Date("2014-04-12"), as.Date("2014-06-12"),
    3,                     5,           as.Date("2014-04-12"), as.Date("2014-06-12")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A+B-A+B")

  DBI::dbDisconnect(con)
})

test_that("A-A+B-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     5,           as.Date("2014-01-15"), as.Date("2014-02-15")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 5,
    minPostCombinationDuration = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A+B-B")

  DBI::dbDisconnect(con)
})

test_that("A+C-A+B+C-A+C", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-01-20"),
    4,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 5,
    minPostCombinationDuration = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A+C-A+B+C-A+C")

  DBI::dbDisconnect(con)
})

test_that("A-A+B+C-A+C-C", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-01-20"),
    4,                     5,           as.Date("2014-01-10"), as.Date("2014-03-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 1,
    eraCollapseSize = 5,
    combinationWindow = 5,
    minPostCombinationDuration = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A+B+C-A+C-C")

  DBI::dbDisconnect(con)
})

test_that("A-A+C-C-B+C-C", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-15"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-01-14"),
    3,                     5,           as.Date("2014-01-16"), as.Date("2014-01-18"),
    4,                     5,           as.Date("2014-01-09"), as.Date("2014-01-18"),
    4,                     5,           as.Date("2014-03-09"), as.Date("2014-03-30")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 1,
    minPostCombinationDuration = 1,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A+C-C-B+C-C")

  DBI::dbDisconnect(con)
})

test_that("start event == start target", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-10-10"), as.Date("2015-07-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A")

  DBI::dbDisconnect(con)
})

test_that("end event == end target", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-10-15"), as.Date("2015-08-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A")

  DBI::dbDisconnect(con)
})

test_that("start-end event == start-end target", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A")

  DBI::dbDisconnect(con)
})

test_that("start event < start target", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-09-10"), as.Date("2015-08-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  expect_message(
    TreatmentPatterns::export(andromeda, minCellCount = 1),
    "Treatment History table is empty. Nothing to export."
  )

  DBI::dbDisconnect(con)
})

test_that("start event < start target, periodPrior = -60", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-09-10"), as.Date("2015-08-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    startAnchor = "startDate",
    windowStart = -60,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A")

  DBI::dbDisconnect(con)
})

test_that("start event > end target", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("X", "A"),
    type = c("target", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2015-08-02"), as.Date("2015-10-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  expect_message(
    TreatmentPatterns::export(andromeda, minCellCount = 1),
    "Treatment History table is empty. Nothing to export."
  )

  DBI::dbDisconnect(con)
})

test_that("collapse A-B-B-B to A-A+B-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
    2,                     5,           as.Date("2014-05-20"), as.Date("2014-08-17"),
    3,                     5,           as.Date("2014-07-08"), as.Date("2014-08-25"),
    3,                     5,           as.Date("2014-09-19"), as.Date("2015-02-22"),
    3,                     5,           as.Date("2015-02-28"), as.Date("2015-09-26")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    tempEmulationSchema = NULL,
    minEraDuration = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A+B-B")

  DBI::dbDisconnect(con)
})

test_that("collapse A-B-B-B to A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     5,           as.Date("2014-02-03"), as.Date("2014-03-01"),
    3,                     5,           as.Date("2014-03-03"), as.Date("2014-04-01"),
    3,                     5,           as.Date("2014-04-03"), as.Date("2014-05-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    tempEmulationSchema = NULL,
    minEraDuration = 14,
    eraCollapseSize = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})

test_that("collapse A-A-B-B to A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    2,                     5,           as.Date("2014-02-03"), as.Date("2014-03-01"),
    3,                     5,           as.Date("2014-03-03"), as.Date("2014-04-01"),
    3,                     5,           as.Date("2014-04-03"), as.Date("2014-05-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    tempEmulationSchema = NULL,
    minEraDuration = 14,
    eraCollapseSize = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})

test_that("collapse A-A-B-B to A-B 2", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
    2,                     5,           as.Date("2014-02-03"), as.Date("2014-03-01"),
    3,                     5,           as.Date("2014-03-03"), as.Date("2014-04-01"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     5,           as.Date("2014-04-03"), as.Date("2014-05-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    tempEmulationSchema = NULL,
    minEraDuration = 14,
    eraCollapseSize = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})

test_that("collapse A-A-B-B to A+B 2", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
    2,                     5,           as.Date("2014-02-01"), as.Date("2014-03-01"),
    3,                     5,           as.Date("2014-02-05"), as.Date("2014-03-03"),
    2,                     5,           as.Date("2014-03-05"), as.Date("2014-04-01"),
    3,                     5,           as.Date("2014-03-05"), as.Date("2014-04-07")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    tempEmulationSchema = NULL,
    minEraDuration = 14,
    combinationWindow = 14,
    minPostCombinationDuration = 14,
    eraCollapseSize = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A+B")

  DBI::dbDisconnect(con)
})

test_that("collapse: A-A-A-A-A-A-A-A to A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-01-10"),
    2,                     5,           as.Date("2014-01-23"), as.Date("2014-01-24"),
    2,                     5,           as.Date("2014-01-11"), as.Date("2014-01-12"),
    2,                     5,           as.Date("2014-01-13"), as.Date("2014-01-14"),
    2,                     5,           as.Date("2014-01-15"), as.Date("2014-01-16"),
    2,                     5,           as.Date("2014-01-17"), as.Date("2014-01-18"),
    2,                     5,           as.Date("2014-01-19"), as.Date("2014-01-20"),
    2,                     5,           as.Date("2014-05-21"), as.Date("2014-05-22")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    filterTreatments = "First",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A")

  DBI::dbDisconnect(con)
})

test_that("collapse: A-A-A-A-A-A-A-A to A-A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-01-10"),
    2,                     5,           as.Date("2014-01-23"), as.Date("2014-01-24"),
    2,                     5,           as.Date("2014-01-11"), as.Date("2014-01-12"),
    2,                     5,           as.Date("2014-01-13"), as.Date("2014-01-14"),
    2,                     5,           as.Date("2014-01-15"), as.Date("2014-01-16"),
    2,                     5,           as.Date("2014-01-17"), as.Date("2014-01-18"),
    2,                     5,           as.Date("2014-01-19"), as.Date("2014-01-20"),
    2,                     5,           as.Date("2014-05-21"), as.Date("2014-05-22")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A")

  DBI::dbDisconnect(con)
})

test_that("collapse: A-A-A-A-A-A-A-A to A-A-A-A-A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-01-10"),
    2,                     5,           as.Date("2014-01-23"), as.Date("2014-01-24"),
    2,                     5,           as.Date("2014-01-11"), as.Date("2014-01-12"),
    2,                     5,           as.Date("2014-01-13"), as.Date("2014-01-14"),
    2,                     5,           as.Date("2014-01-15"), as.Date("2014-01-16"),
    2,                     5,           as.Date("2014-01-17"), as.Date("2014-01-18"),
    2,                     5,           as.Date("2014-01-19"), as.Date("2014-01-20"),
    2,                     5,           as.Date("2014-05-21"), as.Date("2014-05-22")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 0,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A-A-A-A")

  DBI::dbDisconnect(con)
})

test_that("collapse: A-B-B-A-A-A-A-B to A-B-B-A-A", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-01-10"),
    3,                     5,           as.Date("2014-01-23"), as.Date("2014-01-24"),
    3,                     5,           as.Date("2014-01-11"), as.Date("2014-01-12"),
    3,                     5,           as.Date("2014-01-13"), as.Date("2014-01-14"),
    2,                     5,           as.Date("2014-01-15"), as.Date("2014-01-16"),
    2,                     5,           as.Date("2014-01-17"), as.Date("2014-01-18"),
    2,                     5,           as.Date("2014-01-19"), as.Date("2014-01-20"),
    2,                     5,           as.Date("2014-05-21"), as.Date("2014-05-22")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 0,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-B-A-A")

  DBI::dbDisconnect(con)
})

test_that("FollowUp: A-B-C to A-B-C", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    4,                     5,           as.Date("2014-05-14"), as.Date("2014-07-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-C")

  DBI::dbDisconnect(con)
})

test_that("FollowUp: A-B-C to A-B-C", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    4,                     5,           as.Date("2014-05-14"), as.Date("2014-07-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5,
    startAnchor = "startDate",
    endAnchor = "startDate",
    windowStart = 0,
    windowEnd = 6 * 30
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-C")

  DBI::dbDisconnect(con)
})

test_that("FollowUp: A-B-C to A-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    4,                     5,           as.Date("2014-05-14"), as.Date("2014-07-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5,
    startAnchor = "startDate",
    endAnchor = "startDate",
    windowStart = 0,
    windowEnd = 3 * 30
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B")

  DBI::dbDisconnect(con)
})

test_that("FollowUp: A-B-C to A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("X", "A", "B", "C"),
    type = c("target", "event", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    4,                     5,           as.Date("2014-05-14"), as.Date("2014-07-14")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5,
    startAnchor = "startDate",
    windowStart = 0,
    endAnchor = "startDate",
    windowEnd = 60
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A")

  DBI::dbDisconnect(con)
})

test_that("double Target to 2x A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2014-05-30"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    1,                     5,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    2,                     5,           as.Date("2014-06-10"), as.Date("2014-09-10"),
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5,
    concatTargets = FALSE
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A")
  expect_equal(result$treatment_pathways$freq, 2)

  DBI::dbDisconnect(con)
})

test_that("double Target to A-A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2014-05-30"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    1,                     5,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    2,                     5,           as.Date("2014-06-10"), as.Date("2014-09-10"),
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5,
    concatTargets = TRUE
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A")

  DBI::dbDisconnect(con)
})


test_that("Double target to 2x A-A+B-B", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  skip_on_cran()

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2014-05-30"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     5,           as.Date("2014-01-15"), as.Date("2014-02-15"),
    1,                     5,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    2,                     5,           as.Date("2014-06-01"), as.Date("2014-07-01"),
    3,                     5,           as.Date("2014-06-15"), as.Date("2014-07-15")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 5,
    minPostCombinationDuration = 5,
    filterTreatments = "All",
    maxPathLength = 5,
    concatTargets = FALSE
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-A+B-B")
  expect_equal(result$treatment_pathways$freq, 2)

  DBI::dbDisconnect(con)
})


test_that("Double target 2x: A-B-B-A-A-A-A-B to A-B-B-A-A", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2014-05-30"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-01-10"),
    3,                     5,           as.Date("2014-01-23"), as.Date("2014-01-24"),
    3,                     5,           as.Date("2014-01-11"), as.Date("2014-01-12"),
    3,                     5,           as.Date("2014-01-13"), as.Date("2014-01-14"),
    2,                     5,           as.Date("2014-01-15"), as.Date("2014-01-16"),
    2,                     5,           as.Date("2014-01-17"), as.Date("2014-01-18"),
    2,                     5,           as.Date("2014-01-19"), as.Date("2014-01-20"),
    2,                     5,           as.Date("2014-05-21"), as.Date("2014-05-22"),
    1,                     5,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    2,                     5,           as.Date("2014-06-10"), as.Date("2014-06-10"),
    3,                     5,           as.Date("2014-06-23"), as.Date("2014-06-24"),
    3,                     5,           as.Date("2014-06-11"), as.Date("2014-06-12"),
    3,                     5,           as.Date("2014-06-13"), as.Date("2014-06-14"),
    2,                     5,           as.Date("2014-06-15"), as.Date("2014-06-16"),
    2,                     5,           as.Date("2014-06-17"), as.Date("2014-06-18"),
    2,                     5,           as.Date("2014-06-19"), as.Date("2014-06-20"),
    2,                     5,           as.Date("2014-11-21"), as.Date("2014-11-22")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 0,
    filterTreatments = "All",
    maxPathLength = 5,
    concatTargets = FALSE
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-B-A-A")
  expect_equal(result$treatment_pathways$freq, 2)

  DBI::dbDisconnect(con)
})

test_that("Double target 2x: A-B-B-A-A-A-A-B-A-B-B-A-A-A-A-B", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2014-05-30"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-01-10"),
    3,                     5,           as.Date("2014-01-23"), as.Date("2014-01-24"),
    3,                     5,           as.Date("2014-01-11"), as.Date("2014-01-12"),
    3,                     5,           as.Date("2014-01-13"), as.Date("2014-01-14"),
    2,                     5,           as.Date("2014-01-15"), as.Date("2014-01-16"),
    2,                     5,           as.Date("2014-01-17"), as.Date("2014-01-18"),
    2,                     5,           as.Date("2014-01-19"), as.Date("2014-01-20"),
    2,                     5,           as.Date("2014-05-21"), as.Date("2014-05-22"),
    1,                     5,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    2,                     5,           as.Date("2014-06-10"), as.Date("2014-06-10"),
    3,                     5,           as.Date("2014-06-23"), as.Date("2014-06-24"),
    3,                     5,           as.Date("2014-06-11"), as.Date("2014-06-12"),
    3,                     5,           as.Date("2014-06-13"), as.Date("2014-06-14"),
    2,                     5,           as.Date("2014-06-15"), as.Date("2014-06-16"),
    2,                     5,           as.Date("2014-06-17"), as.Date("2014-06-18"),
    2,                     5,           as.Date("2014-06-19"), as.Date("2014-06-20"),
    2,                     5,           as.Date("2014-11-21"), as.Date("2014-11-22")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 0,
    filterTreatments = "All",
    maxPathLength = 9999,
    concatTargets = TRUE
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  expect_identical(result$treatment_pathways$pathway, "A-B-B-A-A-A-B-A-A-B-B-A-A-A-B-A")

  DBI::dbDisconnect(con)
})

test_that("Double target to 2x A-A+B-B", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2014-05-30"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     5,           as.Date("2014-01-15"), as.Date("2014-02-15"),
    1,                     5,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    2,                     5,           as.Date("2014-06-01"), as.Date("2014-07-01"),
    3,                     5,           as.Date("2014-06-15"), as.Date("2014-07-15"),
    1,                     1,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    1,                     3,           as.Date("2014-06-01"), as.Date("2014-12-31"),
    1,                     7,           as.Date("2014-01-01"), as.Date("2014-05-30"),
    1,                     7,           as.Date("2014-06-01"), as.Date("2014-12-31")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 5,
    minPostCombinationDuration = 5,
    filterTreatments = "All",
    maxPathLength = 5,
    concatTargets = FALSE
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1, nonePaths = TRUE)

  expect_identical(result$treatment_pathways$pathway, c("None", "A-A+B-B"))
  expect_equal(result$treatment_pathways$freq, c(3, 2))

  DBI::dbDisconnect(con)
})
