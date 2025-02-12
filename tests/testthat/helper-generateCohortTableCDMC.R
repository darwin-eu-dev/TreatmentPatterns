generateCohortTableCDMC <- function() {
  if (ableToRun()$CDMC) {
    cohortTableName <- "cohort_table"
    
    con <- DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = CDMConnector::eunomiaDir()
    )
    
    cdm <- CDMConnector::cdmFromCon(
      con = con,
      cdmSchema = "main",
      writeSchema = "main"
    )
    
    ## Read in cohort set ----
    cohortsSet <- CDMConnector::readCohortSet(
      path = system.file(package = "TreatmentPatterns", "exampleCohorts")
    )
    
    ## Generate cohot set ----
    cdm <- CDMConnector::generateCohortSet(
      cdm = cdm,
      cohortSet = cohortsSet,
      name = cohortTableName,
      computeAttrition = FALSE
    )
    
    cohorts <- data.frame(
      cohortId = cohortsSet$cohort_definition_id,
      cohortName = cohortsSet$cohort_name,
      type = c("event", "event", "event", "event", "exit", "event", "event", "target")
    )
    return(list(
      cdm = cdm,
      cohorts = cohorts,
      cohortTableName = cohortTableName,
      con = con
    ))
  } else {
    return(NULL)
  }
}
