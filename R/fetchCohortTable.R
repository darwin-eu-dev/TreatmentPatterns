addAgeSex <- function(tbl, con, cdmSchema) {
  tbl <- tbl |>
    dplyr::rename_with(tolower)

  tblCols <- colnames(tbl)

  person <- attachTable(
    con = con,
    catalog = getCatalog(cdmSchema),
    schema = getSchema(cdmSchema),
    table = "person"
  )

  tbl |>
    dplyr::left_join(person, by = dplyr::join_by(subject_id == person_id)) |>
    dplyr::mutate(sex = dplyr::case_when(
      .data$gender_concept_id == 8532 ~ "Female",
      .data$gender_concept_id == 8507 ~ "Male",
      .default = "NA"
    )) |>
    dplyr::select(dplyr::any_of(tblCols), "year_of_birth", "sex")
}

intToDate <- function(andromeda, tbl, col) {
  type <- andromeda[[tbl]] |>
    head() |>
    dplyr::pull(dplyr::any_of(col)) |>
    class()

  if (type == "integer") {
    andromeda[[tbl]] <- andromeda[[tbl]] |>
      dplyr::mutate(
        !!rlang::sym(col) := dplyr::sql(paste0(sprintf("strftime(TO_TIMESTAMP(%s)", col), ", '%Y-%m-%d')::DATE"))
      )
    appendLog(andromeda, sprintf("Converted `%s` to date in %s", col, tbl))
  } else {
    return(andromeda)
  }
}

fixDates <- function(andromeda) {
  startType <- andromeda$cohort_table |>
    head() |>
    dplyr::pull(.data$cohort_start_date) |>
    class()
  
  endType <- andromeda$cohort_table |>
    head() |>
    dplyr::pull(.data$cohort_end_date) |>
    class()
  
  if (startType == "integer" & endType == "integer") {
    andromeda$cohort_table |>
      dplyr::mutate(
        cohort_start_date = dplyr::sql("strftime(TO_TIMESTAMP(cohort_start_date), '%Y-%m-%d')::DATE"),
        cohort_end_date = dplyr::sql("strftime(TO_TIMESTAMP(cohort_end_date), '%Y-%m-%d')::DATE")
      )
  }

  appendLog(andromeda, "Dropped dbplyr temp tables from Andromeda")

  return(invisible(andromeda))
}

updateSubjectId <- function(aCohortTable) {
  aCohortTable |>
    dplyr::mutate(subject_id = dplyr::sql("dense_rank() OVER (ORDER BY subject_id_origin)"))
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

getSchema <- function(x) {
  items <- attr(x, "name") |>
    as.list()
  
  if (is.null(names(items))) {
    if (length(items) > 1) {
      items[[2]]
    } else {
      items[[1]]
    }
  } else {
    items$schema
  }
}

getCatalog <- function(x) {
  items <- attr(x, "name") |>
    as.list()
  
  if (is.null(names(items))) {
    if (length(items) > 1) {
      items[[1]]
    }
  } else {
    items$catalog
  }
}

attachTable <- function(con, catalog, schema, table) {
  tbl <- tryCatch({
    dplyr::tbl(con, DBI::Id(catalog = catalog, schema = schema, table = table))
  }, error = function(e) {
    dplyr::tbl(con, DBI::Id(catalog = catalog, schema = schema, table = toupper(table)))
  })
  tbl |>
    dplyr::rename_with(tolower)
}

#' fetchCohortTable
#'
#' @param cdm (`cdm_reference`) A CDM reference object.
#' @param connectionDetails (`ConnectionDetails`) Connection details to
#' establish a database connection with using DatabaseConnector.
#' @param connection (`connection`) An already established connection to the databas.
#' @param cdmSchema (`character(1)`) Schema where the OMOP CDM resides.
#' @param writeSchema (`character(1)`) Schema where to write temporary tables to, and where the `cohortTable` exists.
#' @param cohorts (`data.frame`) A data.farme containing atleast the columns
#' `cohortId`, `cohortName`, and `type`. `cohortId`
#' refers to the `cohort_definition_id` in the cohort table in the database.
#' `cohortName` refers to the name of the cohort. These two columns are
#' usually parsed out from a 'cohort set'. The `type` column refers to what
#' 'type' a cohort is. Either `"event"` or `"target"`. An `"event"` cohort
#' will be used to build the pathways with. The `"target"` cohort can be
#' compared to an `indication` cohort (like in CohortMethod or
#' SelfControlledCaseSeries). This cohort is used to nest the events, to make
#' sure the events only happen during a period of interrest (i.e. some disease
#' diagnosis).
#' @param cohortTables (`character(n)`) Character vector of the names of the
#' cohort tables to use. May be more than one table name.
#'
#' @returns `Andromeda`
#'
#' @noRd
#' @noMd
#'
#' @examples
#' if (interactive()) {
#'   cohorts <- data.frame(
#'     cohort_definition_id = c(1, 2, 3),
#'     cohort_name = c("A", "B", "C"),
#'     type = c("event", "event", "target"),
#'   )
#'
#'   fetchCohortTable(
#'     cdm = cdm,
#'     cohorts = cohorts,
#'     cohortTables = c(cohort_table_1, cohort_table_2),
#'   )
#'
#'   fetchCohortTable(
#'     cdm = cdm,
#'     cohorts = cohorts,
#'     cohortTables = c(cohort_table_1, cohort_table_2),
#'   )
#' }
fetchCohortTable <- function(
    cdm = NULL,
    connectionDetails = NULL,
    connection = NULL,
    cdmSchema = NULL,
    writeSchema = NULL,
    cohorts,
    cohortTables
  ) {
  cdmSchema <- if (!is.null(cdm) & is.null(cdmSchema)) {
    cdmSchema <- attr(cdm, "cdm_schema") |>
      as.list() |>
      do.call(what = DBI::Id)
  } else if (is.null(cdm) & !is.null(cdmSchema)) {
    makeSchemaId(cdmSchema)
  }
  
  writeSchema <- if (!is.null(cdm) & is.null(writeSchema)) {
    writeSchemaRef <- attr(cdm, "write_schema")
    tblPrefix <- as.list(writeSchemaRef)$prefix
    writeSchemaRef[names(writeSchemaRef) %in% c("catalog", "schema")] |>
      as.list() |>
      do.call(what = DBI::Id)
  } else if (is.null(cdm) & !is.null(writeSchema)) {
    makeSchemaId(writeSchema)
  }

  cohortTables <- cohortTables |>
    purrr::map(\(x) c(attr(writeSchema, "name"), table = x)) |>
    purrr::map(DBI::Id)

  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(
    x = cdm,
    classes = "cdm_reference",
    null.ok = TRUE,
    add = assertions
  )
  checkmate::assertClass(
    x = connectionDetails,
    classes = "ConnectionDetails",
    null.ok = TRUE,
    add = assertions
  )
  checkmate::assertList(
    x = cohortTables,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    null.ok = FALSE,
    add = assertions
  )
  if (!is.null(connection)) {
    checkmate::assertTRUE(
      x = DBI::dbIsValid(connection),
      na.ok = FALSE,
      .var.name = "connection",
      add = assertions 
    )
  }
  checkmate::reportAssertions(assertions)
  
  andromeda <- Andromeda::andromeda() |>
    initLog() |>
    initMetadata() |>
    initAttrition()

  con <- if (!is.null(connection)) {
    appendLog(andromeda, "Using established DBI connection")
    connection
  } else {
    conInterface(connectionDetails, cdm, andromeda)
  }

  if (!is.null(connectionDetails)) {
    on.exit(DBI::dbDisconnect(con))
  }

  cohorts <- cohorts |>
    dplyr::select(
      cohort_definition_id = "cohortId",
      cohort_name = "cohortName",
      "type"
    )

  copyMap <- list(
    Snowflake = TRUE,
    other = "inline"
  )

  copy <- if (!is.null(cdm)) {
    db <- class(attr(cdm, "dbcon"))
    if (!db %in% names(copyMap)) {
      db <- "other"
    }
    copyMap[[db]]
  } else {
    copyMap$other
  }

  cohortTables |>
    purrr::map(dplyr::tbl, src = con) |>
    purrr::map(addAgeSex, con = con, cdmSchema = cdmSchema) |>
    purrr::map(dplyr::rename_with, .fn = tolower) |>
    purrr::map(dplyr::right_join, y = cohorts, by = "cohort_definition_id", copy = copy) |>
    purrr::reduce(dplyr::union_all) |>
    dplyr::mutate(subject_id_origin = as.character(.data$subject_id)) |>
    dplyr::copy_to(dest = andromeda, name = "cohort_table")
  appendLog(andromeda, "Joined `cohorts` to cohort tables")
  appendLog(andromeda, "Saved original `subject_id` as `org_subject_id` as VARCHAR")
  appendLog(andromeda, "Copied merged cohort table to Andromeda as `cohort_table`")

  intToDate(andromeda, tbl = "cohort_table", col = "cohort_start_date")
  intToDate(andromeda, tbl = "cohort_table", col = "cohort_end_date")

  andromeda$cohort_table <- andromeda$cohort_table |>
    dplyr::mutate(age = dplyr::sql("year(cohort_start_date) - year_of_birth")) |>
    dplyr::select(-"year_of_birth")

  n <- andromeda$cohort_table |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(n = as.integer(dplyr::n())) |>
    dplyr::pull(.data$n)

  appendAttrition(
    toAdd = data.frame(
      number_records = as.integer(sum(n)),
      number_subjects = as.integer(length(n)),
      reason_id = 1,
      reason = "Initial qualifying events",
      time_stamp = as.numeric(Sys.time())
    ),
    andromeda = andromeda
  )

  attachTable(con, catalog = getCatalog(cdmSchema), schema = getSchema(cdmSchema), "cdm_source") |>
    dplyr::copy_to(dest = andromeda, name = "cdm_source")
  appendLog(andromeda, "Copied `cdm_source` to Andromeda")

  andromeda$cohort_table <- andromeda$cohort_table |>
    updateSubjectId() |>
    dplyr::compute()
  appendLog(andromeda, "Re-assigned `subject_id` to be 32-bit integers based on `org_subject_id`")

  appendLog(andromeda, "Dropped dbplyr temp tables from Andromeda")

  tmpTables <- names(andromeda)[grepl(pattern = "^dbplyr_", names(andromeda))]
  tmpTables |>
    purrr::map(\(tblName) {
      andromeda[[tblName]] <- NULL
    })

  return(andromeda)
}
