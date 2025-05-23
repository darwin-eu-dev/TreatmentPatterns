library(testthat)
library(TreatmentPatterns)
library(dplyr)

getValidPersonIds <- function(cdm) {
  cdm$observation_period %>%
    inner_join(cdm$person, join_by(person_id == person_id)) %>%
    filter(
      .data$observation_period_start_date <= as.Date("2011-12-31"),
      .data$observation_period_end_date >= as.Date("2016-01-01")
    ) %>%
    collect() %>%
    arrange(.data$person_id) %>%
    pull(.data$person_id)
}

test_that("Pathways", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  on.exit(DBI::dbDisconnect(con))

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4, 5),
    cohortName = c("X", "A", "B", "C", "Y"),
    type = c("target", "event", "event", "event", "target")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    # (X) A
    1, 1, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 1, as.Date("2014-01-10"), as.Date("2014-03-10"),

    # (Y) A-B
    5, 3, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 3, as.Date("2014-01-10"), as.Date("2014-03-10"),
    3, 3, as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (X) A-B-C
    1, 5, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 5, as.Date("2014-01-10"), as.Date("2014-03-10"),
    3, 5, as.Date("2014-03-12"), as.Date("2014-05-12"),
    4, 5, as.Date("2014-05-14"), as.Date("2014-07-14"),

    # (Y) A-B+C
    5, 7, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 7, as.Date("2014-01-03"), as.Date("2014-03-02"),
    3, 7, as.Date("2014-03-10"), as.Date("2014-05-10"),
    4, 7, as.Date("2014-03-10"), as.Date("2014-05-10"),

    # (X) A+B-C
    1, 9, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 9, as.Date("2014-01-10"), as.Date("2014-03-10"),
    3, 9, as.Date("2014-01-10"), as.Date("2014-03-10"),
    4, 9, as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (Y) A-A+B
    5, 11, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 11, as.Date("2014-01-03"), as.Date("2014-03-02"),
    2, 11, as.Date("2014-03-10"), as.Date("2014-05-10"),
    3, 11, as.Date("2014-03-10"), as.Date("2014-05-10"),

    # (X) A-B-A-B
    1, 12, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 12, as.Date("2014-01-03"), as.Date("2014-03-02"),
    3, 12, as.Date("2014-03-10"), as.Date("2014-05-10"),
    2, 12, as.Date("2014-05-12"), as.Date("2014-07-12"),
    3, 12, as.Date("2014-07-14"), as.Date("2014-09-14"),

    # (Y) A-B-A
    5, 16, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 16, as.Date("2014-01-03"), as.Date("2014-03-02"),
    3, 16, as.Date("2014-03-10"), as.Date("2014-05-10"),
    2, 16, as.Date("2014-05-12"), as.Date("2014-07-12"),

    # (X) A-B
    1, 17, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 17, as.Date("2014-01-03"), as.Date("2014-03-02"),
    3, 17, as.Date("2014-03-10"), as.Date("2014-05-10"),
    3, 17, as.Date("2014-05-12"), as.Date("2014-06-12"),

    # (Y) A-B-B
    5, 18, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 18, as.Date("2014-01-03"), as.Date("2014-03-02"),
    3, 18, as.Date("2014-03-10"), as.Date("2014-05-10"),
    3, 18, as.Date("2014-06-12"), as.Date("2014-07-12"),

    # (X) A+B
    1, 19, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 19, as.Date("2014-01-10"), as.Date("2014-03-10"),
    3, 19, as.Date("2014-01-10"), as.Date("2014-03-10"),
    2, 19, as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (Y) A-A-B
    5, 23, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 23, as.Date("2014-01-10"), as.Date("2014-03-10"),
    2, 23, as.Date("2014-04-12"), as.Date("2014-06-12"),
    3, 23, as.Date("2014-06-14"), as.Date("2014-08-14"),

    # (X) A-B
    1, 28, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 28, as.Date("2014-01-10"), as.Date("2014-03-10"),
    2, 28, as.Date("2014-03-12"), as.Date("2014-05-12"),
    3, 28, as.Date("2014-05-14"), as.Date("2014-06-14"),

    # (Y) A+B
    5, 30, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 30, as.Date("2014-01-10"), as.Date("2014-03-10"),
    3, 30, as.Date("2014-01-10"), as.Date("2014-03-10"),
    2, 30, as.Date("2014-03-12"), as.Date("2014-05-12"),
    3, 30, as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (X) A+B
    1, 35, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 35, as.Date("2014-01-10"), as.Date("2014-03-10"),
    3, 35, as.Date("2014-01-10"), as.Date("2014-03-10"),
    2, 35, as.Date("2014-04-12"), as.Date("2014-06-12"),
    3, 35, as.Date("2014-04-12"), as.Date("2014-06-12"),

    # (Y) A-A+B-B
    5, 36, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 36, as.Date("2014-01-01"), as.Date("2014-02-01"),
    3, 36, as.Date("2014-01-15"), as.Date("2014-02-15"),

    # (X) A-A+B-B+C-C
    1, 38, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 38, as.Date("2014-01-01"), as.Date("2014-02-01"),
    3, 38, as.Date("2014-01-10"), as.Date("2014-01-20"),
    4, 38, as.Date("2014-01-01"), as.Date("2014-02-01"),

    # (Y) A-A+B-B+C-C
    5, 40, as.Date("2014-01-01"), as.Date("2015-01-01"),
    2, 40, as.Date("2014-01-01"), as.Date("2014-02-01"),
    3, 40, as.Date("2014-01-10"), as.Date("2014-01-20"),
    4, 40, as.Date("2014-01-10"), as.Date("2014-03-01"),

    # (X) A-A+C-C-B+C-C
    1, 41, as.Date("2014-01-01"), as.Date("2015-01-15"),
    2, 41, as.Date("2014-01-01"), as.Date("2014-01-14"),
    3, 41, as.Date("2014-01-16"), as.Date("2014-01-18"),
    4, 41, as.Date("2014-01-09"), as.Date("2014-01-18"),
    4, 41, as.Date("2014-03-09"), as.Date("2014-03-30")
  )

  suppressWarnings({
    copy_to(con, cohort_table, overwrite = TRUE)
    cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  })

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 1,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  subjectIds <- cohort_table %>%
    pull(.data$subject_id) %>%
    unique()

  DBI::dbDisconnect(con)

  for (subjectId in subjectIds) {
    tbl <- cohort_table %>%
      filter(.data$subject_id == subjectId)

    suppressWarnings({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
      copy_to(con, tbl, overwrite = TRUE)
      cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "tbl", .softValidation = TRUE)

      outputEnv <- TreatmentPatterns::computePathways(
        cohorts = cohorts,
        cohortTableName = "tbl",
        cdm = cdm,
        minEraDuration = 0,
        eraCollapseSize = 30,
        combinationWindow = 30,
        minPostCombinationDuration = 1,
        filterTreatments = "All",
        maxPathLength = 5
      )

      res <- TreatmentPatterns::export(outputEnv, minCellCount = 1)
    })

    target <- res$treatment_pathways %>%
      dplyr::pull(.data$target_cohort_id)

    pathway <- res$treatment_pathways %>%
      dplyr::pull(.data$pathway)

    nRow <- result$treatment_pathways %>%
      dplyr::filter(
        .data$target_cohort_id == target,
        .data$pathway == !!pathway
      ) %>%
      nrow()

    if (interactive()) {
      print(sprintf(">> Subject: %s\n  %s : 1", subjectId, nRow))
    }

    expect_equal(nRow, 1)

    cdm <- CDMConnector::dropSourceTable(cdm = cdm, name = "tbl")
    DBI::dbDisconnect(con)
  }
})

test_that("Events within target", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "Y"),
    type = c("target", "event", "target")
  )

  cohort_table <- dplyr::tribble(
    # (X) A [start event == start target]
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     1,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     1,           as.Date("2014-10-10"), as.Date("2015-07-01"),

    # (Y) A [end event == end target]
    3,                     3,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     3,           as.Date("2014-10-15"), as.Date("2015-08-01"),

    # (X) A [start-end event == start-end target]
    1,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     5,           as.Date("2014-10-10"), as.Date("2015-08-01")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    filterTreatments = "All",
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 1,
    minPostCombinationDuration = 1
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  subjectIds <- cohort_table %>%
    pull(.data$subject_id) %>%
    unique()

  DBI::dbDisconnect(con)

  for (subjectId in subjectIds) {
    tbl <- cohort_table %>%
      filter(.data$subject_id == subjectId)

    suppressWarnings({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
      copy_to(con, tbl, overwrite = TRUE)
      cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "tbl", .softValidation = TRUE)

      outputEnv <- TreatmentPatterns::computePathways(
        cohorts = cohorts,
        cohortTableName = "tbl",
        cdm = cdm,
        filterTreatments = "All",
        minEraDuration = 0,
        eraCollapseSize = 5,
        combinationWindow = 1,
        minPostCombinationDuration = 1
      )

      res <- TreatmentPatterns::export(outputEnv, minCellCount = 1)
    })

    target <- res$treatment_pathways %>%
      dplyr::pull(.data$target_cohort_id)

    pathway <- res$treatment_pathways %>%
      dplyr::pull(.data$pathway)

    result$treatment_pathways %>%
      dplyr::filter(
        .data$target_cohort_id == target,
        .data$pathway == !!pathway
      ) %>%
      nrow() %>%
      expect_equal(1)

    cdm <- CDMConnector::dropSourceTable(cdm = cdm, name = "tbl")
    DBI::dbDisconnect(con)
  }
})

test_that("Events outside target", {
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "Y"),
    type = c("target", "event", "target")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    # (X) None [start event < start target]
    3,                     1,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     1,           as.Date("2014-09-10"), as.Date("2015-08-01"),

    # (Y) None [start event > end target]
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
