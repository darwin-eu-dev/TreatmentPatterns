makeDummyCdm <- function(cohortTable) {
  nPersons <- cohortTable |>
    dplyr::distinct(.data$subject_id) |>
    dplyr::pull(.data$subject_id) |>
    length()
  
  mockCdm <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = nPersons)
  
  cdm <- CDMConnector::copyCdmTo(
    con = DBI::dbConnect(duckdb::duckdb()),
    cdm = mockCdm,
    schema = "main",
    overwrite = TRUE
  )
  
  cdm <- CDMConnector::insertTable(
    cdm = cdm,
    name = "cohort_table",
    table = cohortTable,
    overwrite = TRUE,
    temporary = FALSE
  )
  return(cdm)
}
