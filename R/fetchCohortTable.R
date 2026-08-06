updateSubjectId <- function(aCohortTable) {
  aCohortTable |>
    dplyr::mutate(subject_id = dplyr::sql("dense_rank() OVER (ORDER BY subject_id_org)"))
}

addMinEraDuration <- function(cohortTable, perCohortSetting, minEraDuration) {
  if (!perCohortSetting) {
    cohortTable |>
      dplyr::mutate(min_era_duration = minEraDuration)
  } else {
    cohortTable
  }
}

filterMinEraDuration <- function(cohortTable, perCohortSetting, minEraDuration) {
  cohortTable |>
    addMinEraDuration(perCohortSetting, minEraDuration) %>%
    dplyr::mutate(duration = !!CDMConnector::datediff(start = "cohort_start_date", end = "cohort_end_date")) |>
    dplyr::filter(dplyr::case_when(
      .data$type == "event" ~ .data$duration >= .data$min_era_duration,
      .default = TRUE
    ))
}

checkRows <- function(cohortTables) {
  tableNames <- names(cohortTables)
  
  n <- cohortTables |>
    purrr::map(head) |>
    purrr::map(dplyr::summarise, n = dplyr::n()) |>
    purrr::map(dplyr::pull, "n") |>
    purrr::reduce(max)
  
  if (n == 0) {
    "`%s`" |>
      sprintf(cohortTables) |>
      paste(collapse = ", ") |>
      sprintf(fmt = "Cohort Tables: %s are empty.") |>
      warning()
  }
  
  return(cohortTables)
}

conInterface <- function(connectionDetails = NULL, cdm = NULL, andromeda) {
  if (is.null(connectionDetails) & is.null(cdm)) {
    stop("Neither `connectionDetails` or `cdm` are specified.")
  }
  
  if (!is.null(cdm)) {
    if (!is.null(connectionDetails) & !is.null(cdm)) {
      message("Both `connectionDetails` and `cdm` are specified. Using already open connection from `cdm`")
    }
    appendLog(andromeda, "Using established DBI connection from `cdm_reference`")
    return(attr(cdm, "dbcon"))
  }
  
  if (!is.null(connectionDetails)) {
    con <- DatabaseConnector::dbConnect(
      drv = DatabaseConnector::DatabaseConnectorDriver(),
      connectionDetails = connectionDetails
    )
    appendLog(andromeda, "Opening DBI connection using `DatabaseConnector`")
    return(con)
  }
}

#' fetchCohortTable
#'
#' @param cdm (`cdm_reference`) A CDM reference object.
#' @param connectionDetails (`ConnectionDetails`) Connection details to
#' establish a database connection with using DatabaseConnector.
#' @param dbiConnection (`DBI Connection`) An already established DBI connection.
#' @param cohorts (`data.frame`) A data.farme containing atleast the columns
#' `cohort_definition_id`, `cohort_name`, and `type`. `cohort_definition_id`
#' refers to the `cohort_definition_id` in the cohort table in the database.
#' `cohort_name` refers to the name of the cohort. These two columns are
#' usually parsed out from a 'cohort set'. The `type` column refers to what
#' 'type' a cohort is. Either `"event"` or `"target"`. An `"event"` cohort
#' will be used to build the pathways with. The `"target"` cohort can be
#' compared to an `indication` cohort (like in CohortMethod or
#' SelfControlledCaseSeries). This cohort is used to nest the events, to make
#' sure the events only happen during a period of interrest (i.e. some disease
#' diagnosis). Additional columns that might be added are: `min_era_duration`, ...
#' for per-cohort evaluation of these arguments.
#' @param cohortTables (`character(n)`) Character vector of the names of the
#' cohort tables to use. May be more than one table name.
#' @param minEraDuration (`numeric(1)`) Minimum number of days the cohorts
#' should last.
#' @param perCohortSetting (`logical(1)`: `FALSE`) Allow per cohort evaluation
#' of the `minEraDuration`, if it is added as an extra column to the `cohorts`
#' data.frame.
#'
#' @returns `Andromeda`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   cohorts <- data.frame(
#'     cohort_definition_id = c(1, 2, 3),
#'     cohort_name = c("A", "B", "C"),
#'     type = c("event", "event", "target"),
#'     min_era_duration = c(7, 14, 21),
#'     perCohortSetting = TRUE
#'   )
#'
#'   fetchCohortTable(
#'     cdm = cdm,
#'     cohorts = cohorts,
#'     cohortTables = c(cohort_table_1, cohort_table_2),
#'     minEraDuration = 7
#'     # Apply a `minEraDuration` of `7` across all cohorts
#'     perCohortSetting = FALSE
#'   )
#'
#'   fetchCohortTable(
#'     cdm = cdm,
#'     cohorts = cohorts,
#'     cohortTables = c(cohort_table_1, cohort_table_2),
#'     minEraDuration = 7
#'
#'     # Apply a `minEraDuration` for each cohort seperately, based on the
#'     # `min_era_duration` column in the `cohorts` data.frame.
#'     # `minEraDuration` will be ignored.
#'     perCohortSetting = TRUE
#'   )
#'   
#' }
fetchCohortTable <- function(
    cdm = NULL,
    connectionDetails = NULL,
    dbiConnection = NULL,
    cohorts,
    cohortTables,
    minEraDuration,
    perCohortSetting = FALSE
  ) {
  andromeda <- Andromeda::andromeda() |>
    initLog() |>
    initAttrition()

  con <- if (!is.null(dbiConnection)) {
    dbiConnection
    appendLog(andromeda, "Using established DBI connection")
  } else {
    conInterface(connectionDetails, cdm, andromeda)
  }

  dbCohortTables <- cohortTables |>
    purrr::map(dplyr::tbl, src = con) |>
    purrr::map(dplyr::right_join, y = cohorts, by = "cohort_definition_id", copy = TRUE)
  appendLog(andromeda, "Joined `cohorts` to cohort tables")

  andromeda <- appendAttrition(
    tbl = dbCohortTables,
    andromeda = andromeda,
    reason = "Initial qualifying events",
    reason_id = 1
  )

  dplyr::tbl(con, "cdm_source") |>
    dplyr::copy_to(dest = andromeda, name = "cdm_source")
  appendLog(andromeda, "Copied `cdm_source` to Andromeda")

  tpCohortTable <- dbCohortTables |>
    checkRows() |>
    purrr::map(
      .f = filterMinEraDuration,
      perCohortSetting = perCohortSetting,
      minEraDuration = minEraDuration
    ) |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::compute(name = "tp_cohort_table", temporary = TRUE, overwrite = TRUE)
  appendLog(andromeda, "Applied `minEraDuration` and merged relevant records from cohort tables.")

  andromeda <- appendAttrition(
    tbl = tpCohortTable,
    andromeda = andromeda,
    reason = "applying minEraDuration",
    reason_id = 2
  )

  tpCohortTable |>
    dplyr::mutate(subject_id_org = as.character(.data$subject_id)) |>
    dplyr::copy_to(dest = andromeda, name = "cohort_table", overwrite = TRUE)
  appendLog(andromeda, "Saved original `subject_id` as `org_subject_id` as VARCHAR")
  appendLog(andromeda, "Copied merged cohort table to Andromeda as `cohort_table`")

  DBI::dbRemoveTable(conn = con, name = "tp_cohort_table")
  appendLog(andromeda, "Dropped temp `tp_cohort_table`.")

  andromeda$cohort_table <- andromeda$cohort_table |>
    updateSubjectId() |>
    dplyr::compute()
  appendLog(andromeda, "Re-assigned `subject_id` to be 32-bit integers based on `org_subject_id`")

  tmpTables <- names(andromeda)[grepl(pattern = "^dbplyr_", names(andromeda))]
  tmpTables |>
    purrr::map(\(tblName) {
      andromeda[[tblName]] <- NULL
    })

  appendLog(andromeda, "Dropped dbplyr temp tables from Andromeda")

  return(andromeda)
}
