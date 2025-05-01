if (interactive()) {
  source("./tests/testthat/helper-ableToRun.R")
  source("./tests/testthat/helper-generateCohortTableCDMC.R")
  source("./tests/testthat/helper-generateCohortTableCG.R")
}

library(testthat)
library(TreatmentPatterns)
library(dplyr)

test_that("fetchCohortTable", {
  skip_on_ci()
  skip_on_cran()
  cg <- generateCohortTableCG()
  cdmc <- generateCohortTableCDMC()
  
  aCG <- Andromeda::andromeda()
  aCDMC <- Andromeda::andromeda()
  
  dbcInterface <- TreatmentPatterns:::makeCdmInterface(
    connectionDetails = cg$connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  cdmcInterface <- TreatmentPatterns:::makeCdmInterface(
    cdm = cdmc$cdm
  )
  
  x <- dbcInterface$fetchCohortTable(
    cohorts = cg$cohorts,
    cohortTableName = cg$cohortTableName,
    andromeda = aCG,
    andromedaTableName = cg$cohortTableName
  )
  
  x <- cdmcInterface$fetchCohortTable(
    cohorts = cdmc$cohorts,
    cohortTableName = cdmc$cohortTableName,
    andromeda = aCDMC,
    andromedaTableName = cdmc$cohortTableName
  )
  
  # Check nRows
  expect_identical(
    aCG$cohort_table %>% collect() %>% nrow(),
    aCDMC$cohort_table %>% collect() %>% nrow()
  )
  
  # check n > 1 treatments
  expect_identical(
    aCG$cohort_table %>%
      group_by(.data$subject_id) %>%
      summarize(n = n()) %>%
      filter(n > 1) %>%
      collect() %>%
      mutate(subject_id = as.numeric(subject_id)) %>%
      pull(n) %>%
      sum(),
    aCDMC$cohort_table %>%
      group_by(.data$subject_id) %>%
      summarize(n = n()) %>%
      filter(n > 1) %>%
      collect() %>%
      mutate(subject_id = as.numeric(subject_id)) %>%
      pull(n) %>%
      sum()
  )

  # check n == 1 treatments
  expect_identical(
    aCG$cohort_table %>%
      group_by(.data$subject_id) %>%
      summarize(n = n()) %>%
      filter(n == 1) %>%
      collect() %>%
      mutate(subject_id = as.numeric(subject_id)) %>%
      pull(n) %>%
      sum(),
    aCDMC$cohort_table %>%
      group_by(.data$subject_id) %>%
      summarize(n = n()) %>%
      filter(n == 1) %>%
      collect() %>%
      mutate(subject_id = as.numeric(subject_id)) %>%
      pull(n) %>%
      sum()
  )
  
  dbcInterface$disconnect()
  Andromeda::close(aCG)
  Andromeda::close(aCDMC)
  DBI::dbDisconnect(cdmc$con, shutdown = TRUE)
})
