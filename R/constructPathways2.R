subsetSubjectsAnalysis <- function(andromeda, targetCohortId, analysisCohortTable) {
  selectPeople <- andromeda[[analysisCohortTable]] %>%
    dplyr::filter(.data$cohortId == targetCohortId) %>%
    dplyr::distinct(.data$personId)
  
  andromeda[[sprintf("subjects_%s", args$analysisId)]] <- andromeda[[analysisCohortTable]] %>%
    dplyr::inner_join(selectPeople, by = dplyr::join_by("personId"))
}

createTreatmentHistory <- function(
    andromeda,
    targetCohortId,
    eventCohortIds,
    exitCohortIds,
    indexDateOffset,
    includeTreatments,
    analysisId
  ) {
  cohortTable <- sprintf("cohortTable_%s_%s", analysisId, targetCohortId)
  cohortTable <- sprintf("cohortTable_%s_%s", analysisId, targetCohortId)
  targetCohortTable <- sprintf("targetCohorts_%s", analysisId)
  eventCohortTable <- sprintf("eventCohorts_%s", analysisId)
  exitCohortTable <- sprintf("exitCohorts_%s", analysisId)

  andromeda[[targetCohortTable]] <- andromeda[[cohortTable]] %>%
    dplyr::filter(.data$cohortId %in% targetCohortId) %>%
    dplyr::mutate(
      type = "target",
      indexYear = floor(.data$startDate / 365.25 + 1970),
      indexDate = .data$startDate + indexDateOffset
    )
  
  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  andromeda[["eventCohorts_%s", analysisId]] <- andromeda[[cohortTable]] %>%
    dplyr::filter(.data$cohortId %in% eventCohortIds) %>%
    dplyr::mutate(type = "event")
  
  andromeda[[exitCohortTable]] <- andromeda[[cohortTable]] %>%
    dplyr::filter(.data$cohortId %in% exitCohortIds) %>%
    dplyr::mutate(type = "exit")
  
  nRows <- andromeda[[exitCohortTable]] %>%
    dplyr::count() %>%
    dplyr::pull()
  
  if (nRows > 0) {
    andromeda[[eventCohortTable]] <- andromeda[[eventCohortTable]] %>%
      dplyr::union_all(andromeda[[exitCohortTable]])
  }

  Andromeda::createIndex(andromeda[[eventCohortTable]], c("personId", "startDate", "endDate"))
  Andromeda::createIndex(andromeda[[targetCohortTable]], c("personId", "indexDate", "endDate"))

  # Only keep event cohorts starting (startDate) or ending (endDate) after
  # target cohort start date
  if (includeTreatments == "startDate") {
    andromeda[[sprintf("cohortTable_%s", targetCohortId)]] <- dplyr::full_join(
      x = andromeda$eventCohorts,
      y = andromeda$targetCohorts,
      by = dplyr::join_by(
        personId == personId,
        subject_id_origin == subject_id_origin,
        y$indexDate <= x$startDate,
        x$startDate <= y$endDate,
        x$endDate <= y$endDate,
      ), suffix = c("Event", "Target"))
  } else if (includeTreatments == "endDate") {
    andromeda[[sprintf("cohortTable_%s", targetCohortId)]] <- dplyr::full_join(
      x = andromeda$eventCohorts,
      y = andromeda$targetCohorts,
      by = dplyr::join_by(
        "personId",
        y$indexDate <= x$endDate,
        x$endDate <= y$endDate
      ), suffix = c("Event", "Target")) %>%
      dplyr::mutate(
        startDateEvent = pmax(
          .data$startDateTarget + indexDateOffset,
          .data$startDateEvent,
          na.rm = TRUE
        )
      )
  }
  
  andromeda$treatmentHistory <- andromeda[[sprintf("cohortTable_%s", targetCohortId)]] %>%
    dplyr::select(
      "personId",
      "indexYear",
      eventCohortId = "cohortIdEvent",
      eventStartDate = "startDateEvent",
      eventEndDate = "endDateEvent",
      type = "typeEvent",
      age = "ageEvent",
      sex = "sexEvent",
      targetCohortId = "cohortIdTarget") %>%
    dplyr::mutate(
      durationEra = .data$eventEndDate - .data$eventStartDate,
      eventCohortId = as.character(as.integer(.data$eventCohortId))
    ) %>%
    dplyr::filter(
      !is.na(.data$indexYear),
      !is.na(.data$eventCohortId)
    )
  
  attrCounts <- fetchAttritionCounts(andromeda, "treatmentHistory")
  appendAttrition(
    toAdd = data.frame(
      number_records = attrCounts$nRecords,
      number_subjects = attrCounts$nSubjects,
      reason_id = 3,
      reason = sprintf("Removing events where index date < target index date + indexDateOffset (%s)", indexDateOffset)
    ),
    andromeda = andromeda
  )
  return(invisible(NULL))
}

constructPathwayCohort <- function(targetCohortId, eventCohortIds, exitCohortIds, andromeda, settings, analysisCohortTable) {
  targetCohortName <- andromeda$cohorts %>%
    dplyr::filter(.data$cohortId == targetCohortId) %>%
    dplyr::pull(.data$cohortName)

  message(sprintf(">> Starting on target: %s (%s)", targetCohortId, targetCohortName))

  subsetSubjectsAnalysis(andromeda, targetCohortId, analysisCohortTable)

  createTreatmentHistory(
    andromeda = andromeda,
    targetCohortId = targetCohortId,
    eventCohortIds = eventCohortIds,
    exitCohortIds = exitCohortIds,
    indexDateOffset = settings$indexDateOffset,
    includeTreatments = settings$includeTreatments,
    analysisId = settings$analysisId
  )
}

filterMinEraDuration <- function(andromeda, minEraDuration, analysisCohortTable, generalCohortTable) {
  andromeda[[analysisCohortTable]] <- andromeda[[generalCohortTable]] %>%
    dplyr::filter(.data$endDate - .data$startDate >= minEraDuration)

  attrCounts <- fetchAttritionCounts(andromeda, analysisCohortTable)
  appendAttrition(
    toAdd = data.frame(
      number_records = attrCounts$nRecords,
      number_subjects = attrCounts$nSubjects,
      reason_id = 3,
      reason = sprintf("minEraDuration: %s", minEraDuration)
    ),
    andromeda = andromeda
  )
}

constructPathways <- function(settings, andromeda) {
  andromeda$cohorts <- as.data.frame(settings$cohorts)

  targetCohortIds <- getCohortIds(cohorts = settings$cohorts, cohortType = "target")
  eventCohortIds <- getCohortIds(cohorts = settings$cohorts, cohortType = "event")
  exitCohortIds <- getCohortIds(cohorts = settings$cohorts, cohortType = "exit")

  analysisCohortTable <- sprintf("cohortTable_%s", settings$analysisId)

  filterMinEraDuration(
    andromeda = andromeda,
    minEraDuration = settings$minEraDuration,
    analysisCohortTable = analysisCohortTable,
    generalCohortTable = "cohort_table_all"
  )

  for (targetCohortId in targetCohortIds) {
    constructPathwayCohort(targetCohortId, eventCohortIds, exitCohortIds, andromeda, settings, analysisCohortTable)
  }
}
