# Copyright 2024 DARWIN EU®
#
# This file is part of TreatmentPatterns
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' constructPathway
#'
#' Constructs the pathways.
#'
#' @noRd
#'
#' @param settings (`data.frame`)
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return `invisible(NULL)`
constructPathways <- function(settings, andromeda) {
  andromeda$cohorts <- settings$cohorts %>%
    as.data.frame()

  targetCohortIds <- getCohortIds(cohorts = settings$cohorts, cohortType = "target")
  eventCohortIds <- getCohortIds(cohorts = settings$cohorts, cohortType = "event")
  exitCohortIds <- getCohortIds(cohorts = settings$cohorts, cohortType = "exit")

  for (targetCohortId in targetCohortIds) {
    targetCohortName <- andromeda$cohorts %>%
      dplyr::filter(.data$cohortId == targetCohortId) %>%
      dplyr::pull(.data$cohortName)

    message(sprintf(">> Starting on target: %s (%s)", targetCohortId, targetCohortName))

    selectPeople <- andromeda$cohortTable %>%
      dplyr::filter(.data$cohortId == targetCohortId) %>%
      dplyr::distinct(.data$personId)
  
    andromeda$currentCohorts <- andromeda$cohortTable %>%
      dplyr::inner_join(selectPeople, by = dplyr::join_by("personId"))
  
    # Preprocess the target/event cohorts to create treatment history
    createTreatmentHistory(
      andromeda = andromeda,
      targetCohortId = targetCohortId,
      eventCohortIds = eventCohortIds,
      exitCohortIds = exitCohortIds,
      indexDateOffset = settings$indexDateOffset,
      includeTreatments = settings$includeTreatments
    )

    n <- andromeda$attrition %>%
      dplyr::collect() %>%
      tail(1) %>%
      dplyr::pull(.data$number_records)
  
    if (n > 0) {
      andromeda$exitHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$type == "exit") %>%
        dplyr::select(-"type")
  
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$type == "event")
  
      doSplitEventCohorts(
        andromeda = andromeda,
        splitEventCohorts = settings$splitEventCohorts,
        splitTime = settings$splitTime
      )
  
      doEraCollapse(
        andromeda = andromeda,
        eraCollapseSize = settings$eraCollapseSize
      )
  
      doCombinationWindow(
        andromeda = andromeda,
        combinationWindow = settings$combinationWindow,
        minPostCombinationDuration = settings$minPostCombinationDuration,
        overlapMethod = settings$overlapMethod
      )
  
      doFilterTreatments(
        andromeda = andromeda,
        filterTreatments = settings$filterTreatments
      )
  
      andromeda$exitHistory <- andromeda$exitHistory %>%
        dplyr::mutate(eventCohortId = as.character(as.integer(.data$eventCohortId)))
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::union_all(andromeda$exitHistory)
  
      if (andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% pull() > 0) {
        # Add eventSeq number to determine order of treatments in pathway
        andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
          dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)
  
        andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
          dplyr::group_by(.data$personId) %>%
          dplyr::mutate(eventSeq = dplyr::row_number())
  
        andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
          dplyr::filter(.data$eventSeq <= !!settings$maxPathLength)
  
        # Add event_cohort_name (instead of only event_cohort_id)
        addLabels(andromeda = andromeda)
  
        # Order the combinations
        eventCohortNames <- andromeda$treatmentHistory %>%
          dplyr::select("eventCohortName") %>%
          dplyr::pull() %>%
          stringr::str_split(pattern = "\\+") %>%
          lapply(FUN = function(x) {
            paste(sort(x), collapse = "+")
          }) %>%
          unlist()
  
        andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
          dplyr::collect() %>%
          dplyr::mutate(
            eventCohortName = eventCohortNames,
            indexYear = floor(.data$indexYear / 365.25) + 1970)
      }
    } else {
      warning(sprintf("No cases found for: %s (%s). Generating empty treatmentHistory table.", targetCohortId, targetCohortName))
    }
  
    if (is.null(andromeda$treatmentHistoryFinal)) {
      andromeda$treatmentHistoryFinal <- andromeda$treatmentHistory %>%
            dplyr::filter(!is.na(.data$personId))
    } else {
      andromeda$treatmentHistoryFinal <- andromeda$treatmentHistoryFinal %>%
        dplyr::union_all(
          andromeda$treatmentHistory %>%
            dplyr::filter(!is.na(.data$personId))
        )
    }
  }
  return(andromeda)
}

getCohortIds <- function(cohorts, cohortType) {
  cohorts %>%
    dplyr::filter(.data$type == cohortType) %>%
    dplyr::pull(.data$cohortId)
}

#' createTreatmentHistory
#'
#' @noRd
#'
#' @template param_andromeda
#' @param targetCohortIds (`numeric(n)`)
#' @param eventCohortIds (`numeric(n)`)
#' @param exitCohortIds (`numeric(n)`)
#' @template param_indexDateOffset
#' @template param_includeTreatments
#'
#' @return (`data.frame()`)\cr
#' \enumerate{
#'   \item (`numeric()`) personId
#'   \item (`numeric()`) indexYear
#'   \item (`numeric()`) eventCohortId
#'   \item (`as.Date()`) eventStartDate
#'   \item (`as.Date()`) eventEndDate
#'   \item (`character()`) type
#'   \item (`difftime()`) durationEra
#'   \item (`difftime()`) gapSame
#' }
createTreatmentHistory <- function(
    andromeda,
    targetCohortId,
    eventCohortIds,
    exitCohortIds,
    indexDateOffset,
    includeTreatments) {
  andromeda$targetCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% targetCohortId) %>%
    dplyr::mutate(
      type = "target",
      indexYear = floor(.data$startDate / 365.25 + 1970),
      indexDate = .data$startDate + indexDateOffset
    )
  
  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  andromeda$eventCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% eventCohortIds) %>%
    dplyr::mutate(type = "event")
  
  andromeda$exitCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% exitCohortIds) %>%
    dplyr::mutate(type = "exit")

  nRows <- andromeda$exitCohorts %>%
    dplyr::count() %>%
    dplyr::pull()
  
  if (nRows > 0) {
    andromeda$eventCohorts <- andromeda$eventCohorts %>%
      dplyr::union_all(andromeda$exitCohorts)
  }
  
  Andromeda::createIndex(andromeda$eventCohorts, c("personId", "startDate", "endDate"))
  Andromeda::createIndex(andromeda$targetCohorts, c("personId", "indexDate", "endDate"))
  
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

#' doSplitEventCohorts
#'
#' Splits the treatmentHistory data.frame based on event cohorts into ‘acute’
#' and ‘therapy’ cohorts.
#'
#' @noRd
#'
#' @param splitEventCohorts (`character(n)`)
#'
#' @param splitTime (`integer(1)`)
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
doSplitEventCohorts <- function(
    andromeda,
    splitEventCohorts,
    splitTime) {
  if (!is.null(splitEventCohorts) & !is.null(splitTime)) {
    # Load in labels cohorts
    labels <- andromeda$cohorts %>%
      dplyr::collect()
    
    # Check if splitEventCohorts == splitTime
    checkmate::assertTRUE(length(splitEventCohorts) == length(splitTime))
    
    for (c in seq_len(length(splitEventCohorts))) {
      cohort <- as.character(as.integer(splitEventCohorts[c]))
      cutoff <- splitTime[c]
      
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::mutate(eventCohortId = dplyr::case_when(
          .data$eventCohortId == cohort & .data$durationEra < cutoff ~ paste0(cohort, 1L),
          .data$eventCohortId == cohort & .data$durationEra >= cutoff ~ paste0(cohort, 2L),
          .default = .data$eventCohortId
        ))
      
      # Add new labels
      original <- labels %>%
        dplyr::filter(.data$cohortId == as.integer(cohort))
      
      acute <- original
      acute$cohortId <- as.integer(paste0(cohort, 1))
      acute$cohortName <- sprintf("%s (acute)", acute$cohortName)
      
      therapy <- original
      therapy$cohortId <- as.integer(paste0(cohort, 2))
      therapy$cohortName <- sprintf("%ss (therapy)", therapy$cohortName)
      
      labels <- labels %>%
        dplyr::filter(.data$cohortId != as.integer(cohort))
      
      andromeda$labels <- rbind(labels, acute, therapy)
    }
  } else {
    andromeda$labels <- andromeda$cohorts
  }
  attrCounts <- fetchAttritionCounts(andromeda, "treatmentHistory")
  appendAttrition(
    toAdd = data.frame(
      number_records = attrCounts$nRecords,
      number_subjects = attrCounts$nSubjects,
      reason_id = 4,
      reason = sprintf("splitEventCohorts")
    ),
    andromeda = andromeda
  )
  return(invisible(NULL))
}

#' doEraCollapse
#'
#' Updates the treatmentHistory data.frame where if gapSame is smaller than the
#' specified era collapse size (eraCollapseSize) are collapsed
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#' @param eraCollapseSize (`integer(1)`)
#'
#' @return (`invisible(NULL)`)
doEraCollapse <- function(andromeda, eraCollapseSize) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::group_by(.data$eventCohortId, .data$personId) %>%
    dbplyr::window_order(.data$eventStartDate, .data$eventEndDate) %>%
    dplyr::mutate(
      nextStart = dplyr::lead(.data$eventStartDate),
      nextEnd = dplyr::lead(.data$eventEndDate),
      gap = as.numeric(.data$nextStart - .data$eventEndDate)
    ) %>%
    dplyr::mutate(row_person = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(row = row_number())

  rows <- andromeda$treatmentHistory %>%
    dplyr::group_by(.data$eventCohortId, .data$personId) %>%
    dbplyr::window_order(.data$eventStartDate, .data$eventEndDate) %>%
    dplyr::filter(.data$gap <= eraCollapseSize) %>%
    dplyr::pull(.data$row)
  
  for (row in rows) {
    record <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$eventCohortId, .data$personId) %>%
      dbplyr::window_order(.data$eventStartDate, .data$eventEndDate) %>%
      dplyr::filter(.data$row == !!row)
    
    startDate <- record %>% dplyr::pull(.data$eventStartDate)
    row_person <- record %>% dplyr::pull(.data$row_person)

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$eventCohortId, .data$personId) %>%
      dbplyr::window_order(.data$eventStartDate, .data$eventEndDate) %>%
      dplyr::mutate(
        eventStartDate = dplyr::case_when(
          .data$row_person == !!row_person + 1 & dplyr::lag(.data$row) == !!row ~ startDate,
          .default = .data$eventStartDate
        )
      )
  }

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::filter(!.data$row %in% rows) %>%
    dplyr::select(-"nextStart", -"nextEnd", -"gap", -"row", "row_person")

  attrCounts <- fetchAttritionCounts(andromeda, "treatmentHistory")
  appendAttrition(
    toAdd = data.frame(
      number_records = attrCounts$nRecords,
      number_subjects = attrCounts$nSubjects,
      reason_id = 5,
      reason = sprintf("Collapsing eras, eraCollapse (%s)", eraCollapseSize)
    ),
    andromeda = andromeda
  )
  return(invisible(NULL))
}


#' Combine overlapping events into combinations
#'
#' doCombinationWindow is an internal function that combines overlapping events
#' into combination events. It accepts a treatmentHistory dataframe and returns
#' a modified treatmentHistory dataframe. The returned treatmentHistory
#' dataframe always has the property that a person is only in one event cohort,
#' which might be a combination event cohort, at any point time.
#'
#' @noRd
#'
#' @param combinationWindow (`integer(1)`)
#' @param minPostCombinationDuration (`integer(1)`)
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
doCombinationWindow <- function(
    andromeda,
    combinationWindow,
    minPostCombinationDuration,
    overlapMethod
  ) {
  # Find which rows contain some overlap
  selectRowsCombinationWindow(andromeda, combinationWindow, overlapMethod)

  # While rows that need modification exist:
  iterations <- 1
  
  # n <- andromeda$treatmentHistory %>%
  #   summarise(sum = sum(.data$selectedRows), .groups = "drop") %>%
  #   dplyr::pull()
  
  while (andromeda$treatmentHistory %>%
         dplyr::filter(.data$selectedRows) %>%
         dplyr::count() %>%
         dplyr::pull() != 0) {
    # Which rows have gap previous shorter than combination window OR
    # min(current duration era, previous duration era) -> add column switch
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(switch = case_when(
        .data$selectedRows == 1 &
          -.data$gapPrevious < combinationWindow &
          !(-.data$gapPrevious == .data$durationEra |
              -gapPrevious == dplyr::lag(.data$durationEra, order_by = .data$sortOrder)) ~ 1,
        .default = 0
      ))
    
    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] <=
    # treatmentHistory[r, event_end_date] ->
    # add column combination first received, first stopped
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dplyr::mutate(combinationFRFS = case_when(
        .data$selectedRows == 1 &
          switch == 0 &
          dplyr::lag(eventEndDate, order_by = .data$sortOrder) < eventEndDate ~ 1,
        .default = 0
      )) %>%
      dplyr::ungroup()
    
    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] >
    # treatmentHistory[r, event_end_date] ->
    # add column combination last received, first stopped
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dplyr::mutate(combinationLRFS = dplyr::case_when(
        .data$selectedRows == 1 &
          .data$switch == 0 &
          (dplyr::lag(.data$eventEndDate, order_by = .data$sortOrder) >= .data$eventEndDate |
             (dplyr::lead(.data$durationEra, order_by = .data$sortOrder) == .data$durationEra &
                dplyr::lead(.data$eventEndDate, order_by = .data$sortOrder) == .data$eventEndDate &
                dplyr::lead(.data$eventStartDate, order_by = .data$sortOrder) == .data$eventStartDate)) ~ 1,
        .default = 0
      )) %>%
      dplyr::ungroup()

    sumSwitchComb <- andromeda$treatmentHistory %>%
      dplyr::filter(
        .data$switch == 1 |
          .data$combinationFRFS == 1 |
          .data$combinationLRFS == 1) %>%
      dplyr::summarise(dplyr::n()) %>%
      pull()

    sumSelectedRows <- andromeda$treatmentHistory %>%
      dplyr::summarise(sum = sum(.data$selectedRows, na.rm = TRUE)) %>%
      dplyr::pull()
    
    if (sumSwitchComb != sumSelectedRows) {
      stop(sprintf("Expected switches before combination (%s) to be equal to switches after combination (%s)", sumSelectedRows, sumSwitchComb))
    }
    
    # Do transformations for each of the three newly added columns
    # Construct helpers
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dplyr::mutate(
        eventStartDateNext = dplyr::lead(
          .data$eventStartDate,
          order_by = .data$eventStartDate)) %>%
      dplyr::mutate(
        eventEndDatePrevious = dplyr::lag(
          .data$eventEndDate,
          order_by = .data$eventStartDate)) %>%
      dplyr::mutate(
        eventEndDateNext = dplyr::lead(
          .data$eventEndDate,
          order_by = .data$eventStartDate)) %>%
      dplyr::mutate(
        eventCohortIdPrevious = dplyr::lag(
          .data$eventCohortId,
          order_by = .data$eventStartDate)) %>%
      dplyr::ungroup()

    if (overlapMethod == "truncate") {
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          dplyr::lead(.data$switch) == 1 ~ .data$eventStartDateNext,
          .default = .data$eventEndDate))
    }

    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$combinationFRFS == 1)
    
    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsFRFS_%s", iterations)]] %>%
      dplyr::mutate(eventEndDate = .data$eventEndDatePrevious)

    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsFRFS_%s", iterations)]] %>%
      dplyr::mutate(eventCohortId = paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          dplyr::lead(.data$combinationFRFS) == 1 ~ eventStartDateNext,
          .default = .data$eventEndDate
        ),
        checkDuration = dplyr::case_when(dplyr::lead(.data$combinationFRFS) == 1 ~ 1)
      )
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        eventStartDate = dplyr::case_when(
          .data$combinationFRFS == 1 ~ .data$eventEndDatePrevious,
          .default = .data$eventStartDate
        ),
        checkDuration = dplyr::case_when(
          .data$combinationFRFS == 1 ~ 1,
          .default = .data$checkDuration
        )
      )
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(eventCohortId = dplyr::case_when(
        .data$combinationLRFS == 1 ~ paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious),
        .default = .data$eventCohortId
      ))
    
    andromeda[[sprintf("addRowsLRFS_%s", iterations)]] <- andromeda$treatmentHistory %>%
      dbplyr::window_order(.data$personId, .data$sortOrder) %>%
      dplyr::filter(dplyr::lead(.data$combinationLRFS) == 1)
    
    andromeda[[sprintf("addRowsLRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsLRFS_%s", iterations)]] %>%
      dplyr::mutate(
        eventStartDate = .data$eventEndDateNext,
        checkDuration = 1
      )
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dbplyr::window_order(.data$sortOrder) %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          dplyr::lead(.data$combinationLRFS) == 1 ~ .data$eventStartDateNext,
          .default = .data$eventEndDate
        ),
        checkDuration = dplyr::case_when(
          dplyr::lead(.data$combinationLRFS) == 1 ~ 1,
          .default = .data$checkDuration
        )
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union_all(andromeda[[sprintf("addRowsFRFS_%s", iterations)]]) %>%
      dplyr::union_all(andromeda[[sprintf("addRowsLRFS_%s", iterations)]]) %>%
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      #dplyr::filter(.data$eventStartDate != .data$eventEndDate)
      # Original from mi-erasmus and older versions of DARWIN TreatmentPatterns
      #dbplyr::window_order(.data$sortOrder) %>%
      dplyr::filter(.data$durationEra >= minPostCombinationDuration | is.na(.data$durationEra))

    attrCounts <- fetchAttritionCounts(andromeda, "treatmentHistory")
    appendAttrition(
      toAdd = data.frame(
        number_records = attrCounts$nRecords,
        number_subjects = attrCounts$nSubjects,
        reason_id = 6,
        reason = sprintf("Iteration %s: minPostCombinationDuration (%s), combinatinoWindow (%s)", iterations, minPostCombinationDuration, combinationWindow)
      ),
      andromeda = andromeda
    )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::select(
        "personId", "indexYear", "eventCohortId", "targetCohortId", "eventStartDate", "age",
        "sex", "eventEndDate", "durationEra", "gapPrevious"
      )
    
    selectRowsCombinationWindow(andromeda, combinationWindow, overlapMethod)
    iterations <- iterations + 1
  }
  
  attrCounts <- fetchAttritionCounts(andromeda, "treatmentHistory")
  appendAttrition(
    toAdd = data.frame(
      number_records = attrCounts$nRecords,
      number_subjects = attrCounts$nSubjects,
      reason_id = 7,
      reason = sprintf("After Combination")
    ),
    andromeda = andromeda
  )

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    select(-"gapPrevious", -"selectedRows")
  return(invisible(NULL))
}

#' selectRowsCombinationWindow
#'
#' Help function for doCombinationWindow that selects one overlapping drug era
#' per person to modify in next iteration of the combination window.
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
selectRowsCombinationWindow <- function(andromeda, combinationWindow, overlapMethod) {
  # Order treatmentHistory by person_id, event_start_date, event_end_date
  # andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
  #   arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(sortOrder = as.numeric(.data$eventStartDate) + as.numeric(.data$eventEndDate) * row_number() / n() * 10^-6) %>%
    dplyr::group_by(.data$personId) %>%
    dbplyr::window_order(.data$sortOrder) %>%
    # Use -365 * 1000000 instead of -Inf, because -Inf is not castable for export
    dplyr::mutate(gapPrevious = .data$eventStartDate - dplyr::lag(.data$eventEndDate, order_by = .data$sortOrder, default = -365 * 1000000)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(allRows = ifelse(.data$gapPrevious < 0, dplyr::row_number(), NA)) %>%
    dbplyr::window_order(.data$sortOrder) %>%
    dplyr::mutate(gapPrevious = case_when(
      is.na(.data$gapPrevious) & eventEndDate == lead(eventEndDate) ~ lead(gapPrevious),
      .default = .data$gapPrevious
    ))
  
  #   dplyr::mutate(gapPrevious = .data$eventStartDate - dplyr::lag(.data$eventEndDate)) %>%
  #   ungroup()
  # 
  # # Find all rows with gap_previous < 0
  # andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
  #   dplyr::mutate(allRows = ifelse(.data$gapPrevious < 0, dplyr::row_number(), NA))
  
  # Select one row per iteration for each person
  rows <- andromeda$treatmentHistory %>%
    dplyr::filter(!is.na(.data$allRows)) %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select("allRows") %>%
    dplyr::pull()

  if (overlapMethod == "truncate") {
    treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        selectedRows = dplyr::case_when(
          dplyr::row_number() %in% rows ~ 1,
          .default = 0
        )
      )
  } else if (overlapMethod == "keep") {
    treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        selectedRows = dplyr::case_when(
          dplyr::row_number() %in% rows & -.data$gapPrevious >= combinationWindow ~ 1,
          .default = 0
        )
      )
  }
  
  # treatmentHistory[, ALL_ROWS := NULL]
  andromeda$treatmentHistory <- treatmentHistory %>%
    dplyr::select(-"allRows") %>%
    dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate, .data$eventCohortId)
  return(invisible(NULL))
}


#' doFilterTreatments
#'
#' Updates the treatmentHistory data.frame where the desired event cohorts are
#' maintained for the visualizations
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#' @param filterTreatments (`character(1)`)
#'
#' @return (`invisible(NULL)`)
doFilterTreatments <- function(andromeda, filterTreatments) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dbplyr::window_order(.data$personId, .data$eventStartDate, .data$eventEndDate)

  if (filterTreatments != "All") {
    if (utils::packageVersion("Andromeda") >= package_version("1.0.0")) {
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::mutate(
          eventCohortId = dplyr::sql(
            "list_aggr(list_sort(regexp_split_to_array(eventCohortId, '\\+')::int[]), 'string_agg', '+')"
          )
        )
    } else {
      combi <- grep(
        pattern = "+",
        x = andromeda$treatmentHistory %>%
          dplyr::select("eventCohortId") %>%
          dplyr::pull(), fixed = TRUE
      )
      
      if (length(combi) > 0) {
        mem <- andromeda$treatmentHistory %>%
          dplyr::collect()
        
        conceptIds <- strsplit(
          x = mem$eventCohortId[combi],
          split = "+",
          fixed = TRUE
        )
        
        mem$eventCohortId[combi] <- sapply(
          X = conceptIds,
          FUN = function(x) {
            paste(sort(x), collapse = "+")
          }
        )
        
        andromeda$treatmentHistory <- mem
        rm("mem")
      }
    }
  }

  if (filterTreatments == "First") {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId, .data$eventCohortId) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  } else if (filterTreatments == "Changes") {
    # Group all rows per person for which previous treatment is same
    if (package_version("1.0.0") >= utils::packageVersion("Andromeda")) {
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::group_by(.data$personId, .data$targetCohortId, .data$age, .data$sex, .data$indexYear, .data$eventCohortId, .data$sortOrder) %>%
        dplyr::summarise(
          eventStartDate = min(.data$eventStartDate, na.rm = TRUE),
          eventEndDate = max(.data$eventEndDate, na.rm = TRUE),
          durationEra = sum(.data$durationEra, na.rm = TRUE),
          sortOrder = .data$sortOrder,
          .groups = "drop"
        )
    } else {
      # Group all rows per person for which previous treatment is same
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::collect() %>%
        dplyr::mutate(group = dplyr::consecutive_id(.data$personId, .data$eventCohortId))
      
      # Remove all rows with same sequential treatments
      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::group_by(.data$personId, .data$targetCohortId, .data$age, .data$sex, .data$indexYear, .data$eventCohortId, .data$group, .data$sortOrder) %>%
        dplyr::summarise(
          eventStartDate = min(.data$eventStartDate, na.rm = TRUE),
          eventEndDate = max(.data$eventEndDate, na.rm = TRUE),
          durationEra = sum(.data$durationEra, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(.data$personId, .data$indexYear, .data$group) %>%
        dplyr::select(-"group")
    }
  }
  attrCounts <- fetchAttritionCounts(andromeda, "treatmentHistory")
  appendAttrition(
    toAdd = data.frame(
      number_records = attrCounts$nRecords,
      number_subjects = attrCounts$nSubjects,
      reason_id = 8,
      reason = sprintf("filterTreatments (%s)", filterTreatments)
    ),
    andromeda = andromeda
  )
  return(invisible(NULL))
}

#' addLabels
#'
#' Adds back cohort names to concept ids.
#'
#' @noRd
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (invisible(NULL))
addLabels <- function(andromeda) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(eventCohortId = as.character(.data$eventCohortId))
  
  labels <- if (is.null(andromeda$labels)) {
    andromeda$cohorts %>%
      dplyr::collect() %>%
      dplyr::filter(.data$type != "target") %>%
      dplyr::select("cohortId", "cohortName")
  } else {
    andromeda$labels %>%
      dplyr::collect() %>%
      dplyr::filter(.data$type != "target") %>%
      select("cohortId", "cohortName")
  }
  
  # labels <- labels[labels$cohortType == "event" | labels$cohortType == "exit", c("cohortId", "cohortName")]
  colnames(labels) <- c("eventCohortId", "eventCohortName")
  
  andromeda$labels <- labels %>%
    dplyr::mutate(eventCohortId = as.character(.data$eventCohortId))
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    merge(andromeda$labels, all.x = TRUE, by = "eventCohortId")
  
  mem <- andromeda$treatmentHistory %>%
    dplyr::collect()
  
  mem$eventCohortName[is.na(mem$eventCohortName)] <- sapply(
    X = mem$eventCohortId[is.na(mem$eventCohortName)],
    FUN = function(x) {
      # Reverse search to look for longest concept_ids first
      for (l in rev(seq_len(nrow(labels)))) {
        # If treatment occurs twice in a combination (as monotherapy and as part
        # of fixed-combination) -> remove monotherapy occurrence
        if (any(grep(labels$eventCohortName[l], x))) {
          x <- gsub(labels$eventCohortId[l], "+", x)
        } else {
          x <- gsub(labels$eventCohortId[l], labels$eventCohortName[l], x)
        }
      }
      return(x)
    }
  )
  
  # Filter out + at beginning/end or repetitions
  mem$eventCohortName <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = mem$eventCohortName
  )
  
  andromeda$treatmentHistory <- mem
  rm("mem")
  return(invisible(NULL))
}
