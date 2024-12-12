library(testthat)
library(TreatmentPatterns)
library(dplyr)

test_that("A", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4, 5),
    cohortName = c("X", "A", "B", "C", "Y"),
    type = c("target", "event", "event", "event", "target")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     1,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     1,           as.Date("2014-01-10"), as.Date("2014-03-10"),

    5,                     2,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     2,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     2,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    1,                     3,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     3,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     3,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    4,                     3,           as.Date("2014-05-14"), as.Date("2014-07-14"),

    5,                     4,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     4,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     4,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    4,                     4,           as.Date("2014-03-10"), as.Date("2014-05-10"),

    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    4,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    5,                     6,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     6,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    2,                     6,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     6,           as.Date("2014-03-10"), as.Date("2014-05-10"),

    1,                     7,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     7,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     7,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    2,                     7,           as.Date("2014-05-12"), as.Date("2014-07-12"),
    3,                     7,           as.Date("2014-07-14"), as.Date("2014-09-14"),

    5,                     8,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     8,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     8,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    2,                     8,           as.Date("2014-05-12"), as.Date("2014-07-12"),

    1,                     9,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     9,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     9,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     9,           as.Date("2014-05-12"), as.Date("2014-06-12"),

    5,                     10,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     10,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     10,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     10,           as.Date("2014-06-12"), as.Date("2014-07-12"),

    1,                     11,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     11,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     11,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     11,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    5,                     12,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     12,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     12,           as.Date("2014-04-12"), as.Date("2014-06-12"),
    3,                     12,           as.Date("2014-06-14"), as.Date("2014-08-14"),

    1,                     13,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     13,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     13,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    3,                     13,           as.Date("2014-05-14"), as.Date("2014-06-14"),

    5,                     14,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     14,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     14,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     14,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    3,                     14,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    1,                     15,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     15,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     15,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     15,           as.Date("2014-04-12"), as.Date("2014-06-12"),
    3,                     15,           as.Date("2014-04-12"), as.Date("2014-06-12"),

    5,                     16,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     16,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     16,           as.Date("2014-01-15"), as.Date("2014-02-15"),

    1,                     17,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     17,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     17,           as.Date("2014-01-10"), as.Date("2014-01-20"),
    4,                     17,           as.Date("2014-01-01"), as.Date("2014-02-01"),

    5,                     18,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     18,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     18,           as.Date("2014-01-10"), as.Date("2014-01-20"),
    4,                     18,           as.Date("2014-01-10"), as.Date("2014-03-01"),

    1,                     19,           as.Date("2014-01-01"), as.Date("2015-01-15"),
    2,                     19,           as.Date("2014-01-01"), as.Date("2014-01-14"),
    3,                     19,           as.Date("2014-01-16"), as.Date("2014-01-18"),
    4,                     19,           as.Date("2014-01-09"), as.Date("2014-01-18"),
    4,                     19,           as.Date("2014-03-09"), as.Date("2014-03-30")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table", .softValidation = TRUE)

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    indexDateOffset = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 1,
    minPostCombinationDuration = 1
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  result$treatment_pathways

  expect_equal(
    result$treatment_pathways$pathway,
    c(
     "A-B",
     "A",
     "A+B-A" ,
     "A+B-C" ,
     "A+C-A+B+C" ,
     "A-A+C-C-B+C",
     "A-B-B+C-C",
     "A-B",
     "A-A+B",
     "A-A+B+C-A+C-C",
     "A-A+B-B"
    )
  )

  DBI::dbDisconnect(con)
})

# test_that("start event == start target", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2),
#     cohortName = c("X", "A"),
#     type = c("target", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
#     2,                     5,           as.Date("2014-10-10"), as.Date("2015-07-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = "cohort_table",
#     cdm = cdm,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 0,
#     eraCollapseSize = 30,
#     combinationWindow = 30,
#     minPostCombinationDuration = 30,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("end event == end target", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2),
#     cohortName = c("X", "A"),
#     type = c("target", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
#     2,                     5,           as.Date("2014-10-15"), as.Date("2015-08-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = "cohort_table",
#     cdm = cdm,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 0,
#     eraCollapseSize = 30,
#     combinationWindow = 30,
#     minPostCombinationDuration = 30,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("start-end event == start-end target", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2),
#     cohortName = c("X", "A"),
#     type = c("target", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
#     2,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = "cohort_table",
#     cdm = cdm,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 0,
#     eraCollapseSize = 30,
#     combinationWindow = 30,
#     minPostCombinationDuration = 30,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("start event < start target", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2),
#     cohortName = c("X", "A"),
#     type = c("target", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
#     2,                     5,           as.Date("2014-09-10"), as.Date("2015-08-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = "cohort_table",
#     cdm = cdm,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 0,
#     eraCollapseSize = 30,
#     combinationWindow = 30,
#     minPostCombinationDuration = 30,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )
#   
#   expect_message(
#     TreatmentPatterns::export(andromeda, minCellCount = 1),
#     "Treatment History table is empty. Nothing to export.")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("start event < start target, periodPrior = -60", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2),
#     cohortName = c("X", "A"),
#     type = c("target", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
#     2,                     5,           as.Date("2014-09-10"), as.Date("2015-08-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = "cohort_table",
#     cdm = cdm,
#     includeTreatments = "startDate",
#     indexDateOffset = -60,
#     minEraDuration = 0,
#     eraCollapseSize = 30,
#     combinationWindow = 30,
#     minPostCombinationDuration = 30,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("start event > end target", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2),
#     cohortName = c("X", "A"),
#     type = c("target", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
#     2,                     5,           as.Date("2015-08-01"), as.Date("2015-10-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = "cohort_table",
#     cdm = cdm,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 0,
#     eraCollapseSize = 30,
#     combinationWindow = 30,
#     minPostCombinationDuration = 30,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )
#   
#   expect_message(
#     TreatmentPatterns::export(andromeda, minCellCount = 1),
#     "Treatment History table is empty. Nothing to export."
#   )
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("collapse A-B-B-B to A-A+B-B", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2, 3),
#     cohortName = c("X", "A", "B"),
#     type = c("target", "event", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
#     2,                     5,           as.Date("2014-05-20"), as.Date("2014-08-17"),
#     3,                     5,           as.Date("2014-07-08"), as.Date("2014-08-25"),
#     3,                     5,           as.Date("2014-10-19"), as.Date("2015-02-22"),
#     3,                     5,           as.Date("2015-02-28"), as.Date("2015-09-26")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = 'cohort_table',
#     cdm = cdm,
#     tempEmulationSchema = NULL,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 30,
#     eraCollapseSize = 30,
#     combinationWindow = 30,
#     minPostCombinationDuration = 30,
#     filterTreatments = "All",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A-A+B-B")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("collapse A-B-B-B to A-B", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2, 3),
#     cohortName = c("X", "A", "B"),
#     type = c("target", "event", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
#     2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
#     3,                     5,           as.Date("2014-02-03"), as.Date("2014-03-01"),
#     3,                     5,           as.Date("2014-03-03"), as.Date("2014-04-01"),
#     3,                     5,           as.Date("2014-04-03"), as.Date("2014-05-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = 'cohort_table',
#     cdm = cdm,
#     tempEmulationSchema = NULL,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 14,
#     eraCollapseSize = 5,
#     filterTreatments = "All",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A-B")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("collapse A-A-B-B to A-B", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2, 3),
#     cohortName = c("X", "A", "B"),
#     type = c("target", "event", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
#     2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
#     2,                     5,           as.Date("2014-02-03"), as.Date("2014-03-01"),
#     3,                     5,           as.Date("2014-03-03"), as.Date("2014-04-01"),
#     3,                     5,           as.Date("2014-04-03"), as.Date("2014-05-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = 'cohort_table',
#     cdm = cdm,
#     tempEmulationSchema = NULL,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 14,
#     eraCollapseSize = 5,
#     filterTreatments = "All",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A-B")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("collapse A-A-B-B to A-B 2", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2, 3),
#     cohortName = c("X", "A", "B"),
#     type = c("target", "event", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
#     2,                     5,           as.Date("2014-02-03"), as.Date("2014-03-01"),
#     3,                     5,           as.Date("2014-03-03"), as.Date("2014-04-01"),
#     2,                     5,           as.Date("2014-01-01"), as.Date("2014-02-01"),
#     3,                     5,           as.Date("2014-04-03"), as.Date("2014-05-01")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = 'cohort_table',
#     cdm = cdm,
#     tempEmulationSchema = NULL,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 14,
#     eraCollapseSize = 5,
#     filterTreatments = "All",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A-B")
#   
#   DBI::dbDisconnect(con)
# })
# 
# test_that("collapse A-A-B-B to A+B 2", {
#   skip_if_not(ableToRun()$CDMC)
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#   
#   cohorts <- data.frame(
#     cohortId = c(1, 2, 3),
#     cohortName = c("X", "A", "B"),
#     type = c("target", "event", "event")
#   )
#   
#   cohort_table <- dplyr::tribble(
#     ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
#     1,                     5,           as.Date("2014-01-01"), as.Date("2015-12-31"),
#     2,                     5,           as.Date("2014-02-01"), as.Date("2014-03-01"),
#     3,                     5,           as.Date("2014-02-05"), as.Date("2014-03-03"),
#     2,                     5,           as.Date("2014-03-05"), as.Date("2014-04-01"),
#     3,                     5,           as.Date("2014-03-05"), as.Date("2014-04-07")
#   )
#   
#   copy_to(con, cohort_table, overwrite = TRUE)
#   
#   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
#   
#   andromeda <- TreatmentPatterns::computePathways(
#     cohorts = cohorts,
#     cohortTableName = 'cohort_table',
#     cdm = cdm,
#     tempEmulationSchema = NULL,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 14,
#     combinationWindow = 14,
#     minPostCombinationDuration = 14,
#     eraCollapseSize = 5,
#     filterTreatments = "All",
#     maxPathLength = 5
#   )
#   
#   result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
#   
#   expect_identical(result$treatment_pathways$pathway, "A+B")
#   
#   DBI::dbDisconnect(con)
# })
