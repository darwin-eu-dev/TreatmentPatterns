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

  cohorts <- data.frame(
    cohortId = c(1, 2, 3, 4, 5),
    cohortName = c("X", "A", "B", "C", "Y"),
    type = c("target", "event", "event", "event", "target")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    # (X) A
    1,                     1,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     1,           as.Date("2014-01-10"), as.Date("2014-03-10"),

    # (Y) A-B
    5,                     2,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     2,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     2,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (X) A-B-C
    1,                     3,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     3,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     3,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    4,                     3,           as.Date("2014-05-14"), as.Date("2014-07-14"),

    # (Y) A-B+C
    5,                     4,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     4,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     4,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    4,                     4,           as.Date("2014-03-10"), as.Date("2014-05-10"),

    # (X) A+B-C
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    4,                     5,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (Y) A-A+B
    5,                     6,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     6,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    2,                     6,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     6,           as.Date("2014-03-10"), as.Date("2014-05-10"),

    # (X) A-B-A-B
    1,                     7,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     7,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     7,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    2,                     7,           as.Date("2014-05-12"), as.Date("2014-07-12"),
    3,                     7,           as.Date("2014-07-14"), as.Date("2014-09-14"),

    # (Y) A-B-A
    5,                     8,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     8,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     8,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    2,                     8,           as.Date("2014-05-12"), as.Date("2014-07-12"),

    # (X) A-B
    1,                     9,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     9,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     9,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     9,           as.Date("2014-05-12"), as.Date("2014-06-12"),

    # (Y) A-B-B
    5,                     10,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     10,           as.Date("2014-01-03"), as.Date("2014-03-02"),
    3,                     10,           as.Date("2014-03-10"), as.Date("2014-05-10"),
    3,                     10,           as.Date("2014-06-12"), as.Date("2014-07-12"),

    # (X) A+B
    1,                     11,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     11,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     11,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     11,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (Y) A-A-B
    5,                     12,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     12,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     12,           as.Date("2014-04-12"), as.Date("2014-06-12"),
    3,                     12,           as.Date("2014-06-14"), as.Date("2014-08-14"),

    # (X) A-B
    1,                     13,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     13,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     13,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    3,                     13,           as.Date("2014-05-14"), as.Date("2014-06-14"),

    # (Y) A+B
    5,                     14,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     14,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     14,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     14,           as.Date("2014-03-12"), as.Date("2014-05-12"),
    3,                     14,           as.Date("2014-03-12"), as.Date("2014-05-12"),

    # (X) A+B
    1,                     15,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     15,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    3,                     15,           as.Date("2014-01-10"), as.Date("2014-03-10"),
    2,                     15,           as.Date("2014-04-12"), as.Date("2014-06-12"),
    3,                     15,           as.Date("2014-04-12"), as.Date("2014-06-12"),

    # (Y) A-A+B-B
    5,                     16,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     16,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     16,           as.Date("2014-01-15"), as.Date("2014-02-15"),

    # (X) A-A+B-B+C-C
    1,                     17,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     17,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     17,           as.Date("2014-01-10"), as.Date("2014-01-20"),
    4,                     17,           as.Date("2014-01-01"), as.Date("2014-02-01"),

    # (Y) A-A+B-B+C-C
    5,                     18,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     18,           as.Date("2014-01-01"), as.Date("2014-02-01"),
    3,                     18,           as.Date("2014-01-10"), as.Date("2014-01-20"),
    4,                     18,           as.Date("2014-01-10"), as.Date("2014-03-01"),

    # (X) A-A+C-C-B+C-C
    1,                     19,           as.Date("2014-01-01"), as.Date("2015-01-15"),
    2,                     19,           as.Date("2014-01-01"), as.Date("2014-01-14"),
    3,                     19,           as.Date("2014-01-16"), as.Date("2014-01-18"),
    4,                     19,           as.Date("2014-01-09"), as.Date("2014-01-18"),
    4,                     19,           as.Date("2014-03-09"), as.Date("2014-03-30")
  )

  cdm <- makeDummyCdm(cohort_table)
  on.exit(CDMConnector::cdmDisconnect(cdm))

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    filterTreatments = "All",
    indexDateOffset = 0,
    minEraDuration = 0,
    eraCollapseSize = 5,
    combinationWindow = 1,
    minPostCombinationDuration = 1
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  subjectIds <- cohort_table %>%
    pull(.data$subject_id) %>%
    unique()


  for (subjectId in subjectIds) {
    tbl <- cohort_table %>%
      filter(.data$subject_id == subjectId)

    suppressWarnings({
      cdm2 <- makeDummyCdm(tbl)

      outputEnv <- TreatmentPatterns::computePathways(
        cohorts = cohorts,
        cohortTableName = "cohort_table",
        cdm = cdm2,
        filterTreatments = "All",
        indexDateOffset = 0,
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

    cdm2 <- CDMConnector::dropTable(cdm = cdm2, name = "tbl")
    CDMConnector::cdmDisconnect(cdm2)
  }
})

test_that("Events within target", {
  skip_if_not(ableToRun()$CDMC)
  skip_on_cran()

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
    3,                     2,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     2,           as.Date("2014-10-15"), as.Date("2015-08-01"),

    # (X) A [start-end event == start-end target]
    1,                     3,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     3,           as.Date("2014-10-10"), as.Date("2015-08-01")
  )

  cdm <- makeDummyCdm(cohort_table)

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    indexDateOffset = 0,
    minEraDuration = 0,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)
  
  subjectIds <- cohort_table %>%
    pull(.data$subject_id) %>%
    unique()
  
  CDMConnector::cdmDisconnect(cdm)
  
  for (subjectId in subjectIds) {
    tbl <- cohort_table %>%
      filter(.data$subject_id == subjectId)
    
    suppressWarnings({
      cdm2 <- makeDummyCdm(tbl)
    
      outputEnv <- TreatmentPatterns::computePathways(
        cohorts = cohorts,
        cohortTableName = "cohort_table",
        cdm = cdm2,
        filterTreatments = "All",
        indexDateOffset = 0,
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
    
    cdm2 <- CDMConnector::dropTable(cdm = cdm2, name = "tbl")
    CDMConnector::cdmDisconnect(cdm2)
  }
})

test_that("Events outside target", {
  skip_if_not(ableToRun()$CDMC)

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
    1,                     2,           as.Date("2014-10-10"), as.Date("2015-08-01"),
    2,                     2,           as.Date("2015-08-01"), as.Date("2015-10-01")
  )

  cdm <- makeDummyCdm(cohort_table)

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    indexDateOffset = 0,
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

  CDMConnector::cdmDisconnect(cdm)
})
