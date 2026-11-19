library(CDMConnector)
library(DBI)
library(dplyr)

test_that("Test Database", {
  skip_if_not(file.exists("/home/runner/work/TreatmentPatterns/TreatmentPatterns/args.rds"))

  library(Sys.getenv("DRIVER_PKG"), character.only = TRUE)

  con <- do.call(
    what = DBI::dbConnect,
    args = readRDS("/home/runner/work/TreatmentPatterns/TreatmentPatterns/args.rds")
  )

  cdm <- CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = Sys.getenv("CDM_SCHEMA"),
    writeSchema = Sys.getenv("RESULT_SCHEMA"),
    cdmVersion = "5.3"
  )

  ## Prepare ----
  cohortTableName <- "temp_tp_cohort_table"

  dummyCohortTable <- data.frame(
    cohort_definition_id = 1,
    subject_id = c(1, 2, 3, 4, 5),
    cohort_start_date = as.Date("1999-01-01"),
    cohort_end_date = as.Date("2005-01-01")
  )

  CDMConnector::insertTable(
    cdm = cdm,
    name = cohortTableName,
    table = dummyCohortTable,
    overwrite = TRUE,
    temporary = FALSE
  )

  withr::defer({
    CDMConnector::dropSourceTable(cdm = cdm, name = cohortTableName)
  })

  cohorts <- data.frame(
    cohortId = 1,
    cohortName = "foo",
    type = "target"
  )
  
  andromeda <- TreatmentPatterns:::fetchCohortTable(
    cdm = cdm,
    connection = NULL,
    cdmSchema = NULL,
    writeSchema = NULL,
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
