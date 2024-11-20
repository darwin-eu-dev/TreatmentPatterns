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

  selectPeople <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId == targetCohortIds) %>%
    dplyr::distinct(.data$personId)

  andromeda$currentCohorts <- andromeda$cohortTable %>%
    dplyr::inner_join(selectPeople, by = dplyr::join_by("personId"))

  nRows <- andromeda$cohortTable %>%
    dplyr::count() %>%
    dplyr::pull()

  # Preprocess the target/event cohorts to create treatment history
  createTreatmentHistory(
    andromeda = andromeda,
    targetCohortIds = targetCohortIds,
    eventCohortIds = eventCohortIds,
    exitCohortIds = exitCohortIds,
    indexDateOffset = settings$indexDateOffset,
    includeTreatments = settings$includeTreatments
  )

  if (nRows > 0) {
    andromeda$exitHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "exit") %>%
      dplyr::select(-"type")

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "event")

    # Apply pathway settings to create treatment pathways
    message("Construct treatment pathways, this may take a while for larger datasets.")

    message(sprintf(
      "Original number of rows: %s",
      andromeda$treatmentHistory %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull()
      ))

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
      minPostCombinationDuration = settings$minPostCombinationDuration
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
      message("Adding drug sequence number.")

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::group_by(.data$personId) %>%
        dplyr::mutate(eventSeq = dplyr::row_number())

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$eventSeq <= !!settings$maxPathLength)

      nRows <- andromeda$treatmentHistory %>%
        dplyr::count() %>%
        dplyr::pull()
      message(sprintf("After maxPathLength: %s", nRows))

      # Add event_cohort_name (instead of only event_cohort_id)
      message("Adding concept names.")

      addLabels(andromeda = andromeda)

      # Order the combinations
      message("Ordering the combinations.")

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
    warning("No cases found. Generating empty treatmentHistory table.")
  }

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::filter(!is.na(.data$personId))
  
  message("constructPathways done.")
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
    targetCohortIds,
    eventCohortIds,
    exitCohortIds,
    indexDateOffset,
    includeTreatments) {
  andromeda$targetCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% targetCohortIds) %>%
    dplyr::mutate(type = "target") %>%
    dplyr::mutate(indexYear = as.numeric(format(.data$startDate, "%Y"))) %>%
    dplyr::mutate(indexDate = .data$startDate + indexDateOffset)
  
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
    andromeda$cohortTable <- dplyr::full_join(
      x = andromeda$eventCohorts,
      y = andromeda$targetCohorts,
      by = dplyr::join_by(
        "personId",
        y$indexDate <= x$startDate,
        x$startDate <= y$endDate,
        x$endDate <= y$endDate
      ))
  } else if (includeTreatments == "endDate") {
    andromeda$cohortTable <- dplyr::full_join(
      x = andromeda$eventCohorts,
      y = andromeda$targetCohorts,
      by = dplyr::join_by(
        "personId",
        y$indexDate <= x$endDate,
        x$endDate <= y$endDate
      )) %>%
      dplyr::mutate(
        startDate.x = pmax(
          .data$startDate.y + indexDateOffset,
          .data$startDate.x,
          na.rm = TRUE
        )
      )
  }
  
  andromeda$treatmentHistory <- andromeda$cohortTable %>%
    dplyr::select(
      "personId",
      "indexYear",
      eventCohortId = "cohortId.x",
      eventStartDate = "startDate.x",
      eventEndDate = "endDate.x",
      type = "type.x",
      age = "age.x",
      sex = "sex.x") %>%
    dplyr::mutate(
      durationEra = .data$eventEndDate - .data$eventStartDate,
      eventCohortId = as.character(floor(.data$eventCohortId))) %>%
    dplyr::filter(!is.na(.data$indexYear))
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
  }
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
    dplyr::group_by(.data$personId, .data$eventCohortId) %>%
    dplyr::mutate(
      lagVariable = dplyr::lag(.data$eventEndDate, order_by = .data$eventStartDate)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      needsMerge = .data$eventStartDate - .data$lagVariable < eraCollapseSize,
      rowNumber = dplyr::row_number()) %>%
    dplyr::select(-"lagVariable")
  
  needsMerge <- andromeda$treatmentHistory %>%
    dplyr::filter(.data$needsMerge) %>%
    dplyr::select("rowNumber") %>%
    dplyr::collect() %>%
    dplyr::arrange(.data$rowNumber)
  
  n <- nrow(needsMerge)
  
  # Remove all rows with gap_same < eraCollapseSize
  if (n == 0) {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::select(-"needsMerge", -"rowNumber") %>%
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)
  } else {
    blockEnd <- as.numeric(needsMerge$rowNumber[seq_len(n)] != needsMerge$rowNumber[seq_len(n)]) + 1
    needsMerge$blockId <- cumsum(blockEnd)
    needsMerge <- needsMerge %>%
      dplyr::group_by(.data$blockId) %>%
      dplyr::summarise(
        startRowNumber = min(rowNumber, na.rm = TRUE) - 1,
        endRowNumber = max(rowNumber, na.rm = TRUE),
        .groups = "drop")
    
    newEndDates <- andromeda$treatmentHistory %>%
      dplyr::inner_join(
        needsMerge,
        copy = TRUE,
        by = dplyr::join_by("rowNumber" == "endRowNumber")) %>% 
      dplyr::select("startRowNumber", newEndDate = "eventEndDate")
    
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::left_join(
        newEndDates,
        by = dplyr::join_by("rowNumber" == "startRowNumber")) %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          !is.na(.data$newEndDate) ~ .data$newEndDate,
          .default = .data$eventEndDate
        ),
        needsMerge = dplyr::case_when(
          !is.na(.data$newEndDate) ~ NA,
          .default = .data$needsMerge
        )
      ) %>% 
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate) %>%
      # dplyr::mutate(
      #   eventEndDate = if_else(
      #     is.null(.data$newEndDate), 
      #     .data$eventEndDate, 
      #     .data$newEndDate)) %>%
      dplyr::filter(is.na(.data$needsMerge)) %>%
      dplyr::select(-"newEndDate", -"needsMerge", -"rowNumber") %>%
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)
  }
  message(sprintf("After eraCollapseSize: %s", n))
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
    minPostCombinationDuration) {
  time1 <- Sys.time()
  
  # Find which rows contain some overlap
  selectRowsCombinationWindow(andromeda)
  
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
    treatmentHistory <- andromeda$treatmentHistory %>%
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
    treatmentHistory <- treatmentHistory %>%
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
    andromeda$treatmentHistory <- treatmentHistory %>%
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

    message(sprintf(
      "Selected %s \nout of %s rows\nIteration: %s\nSwitches: %s\nFRFS Combinations: %s\nLRFS Combinations: %s\n",
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$selectedRows, na.rm = TRUE)) %>%
        dplyr::pull(),
      andromeda$treatmentHistory %>%
        dplyr::count() %>%
        dplyr::pull(),
      iterations,
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(!is.na(.data$switch), na.rm = TRUE)) %>%
        dplyr::pull(),
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$combinationFRFS, na.rm = TRUE)) %>%
        dplyr::pull(),
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$combinationLRFS, na.rm = TRUE)) %>%
        dplyr::pull()
    ))
    
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
      dplyr::ungroup() %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          dplyr::lead(.data$switch) == 1 ~ .data$eventStartDateNext,
          .default = .data$eventEndDate))
    
    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$combinationFRFS == 1)
    
    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsFRFS_%s", iterations)]] %>%
      dplyr::mutate(eventEndDate = .data$eventEndDatePrevious)
    
    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsFRFS_%s", iterations)]] %>%
      dplyr::mutate(eventCohortId = paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious))
    
    treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          dplyr::lead(.data$combinationFRFS) == 1 ~ eventStartDateNext,
          .default = .data$eventEndDate
        ),
        checkDuration = dplyr::case_when(dplyr::lead(.data$combinationFRFS) == 1 ~ 1)
      )
    
    treatmentHistory <- treatmentHistory %>%
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
    
    treatmentHistory <- treatmentHistory %>%
      dplyr::mutate(eventCohortId = dplyr::case_when(
        .data$combinationLRFS == 1 ~ paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious),
        .default = .data$eventCohortId
      ))
    
    andromeda[[sprintf("addRowsLRFS_%s", iterations)]] <- andromeda$treatmentHistory %>%
      dplyr::filter(dplyr::lead(.data$combinationLRFS) == 1)
    
    andromeda[[sprintf("addRowsLRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsLRFS_%s", iterations)]] %>%
      dplyr::mutate(
        eventStartDate = .data$eventEndDateNext,
        checkDuration = 1
      )
    
    andromeda$treatmentHistory <- treatmentHistory %>%
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
    
    treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union_all(andromeda[[sprintf("addRowsFRFS_%s", iterations)]]) %>%
      dplyr::union_all(andromeda[[sprintf("addRowsLRFS_%s", iterations)]]) %>%
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)

    treatmentHistory <- treatmentHistory %>%
      #dplyr::filter(.data$eventStartDate != .data$eventEndDate)
      # Original from mi-erasmus and older versions of DARWIN TreatmentPatterns
      #dbplyr::window_order(.data$sortOrder) %>%
      dplyr::filter(.data$durationEra >= minPostCombinationDuration | is.na(.data$durationEra))
    
    andromeda$treatmentHistory <- treatmentHistory %>%
      dplyr::select(
        "personId", "indexYear", "eventCohortId", "eventStartDate", "age",
        "sex", "eventEndDate", "durationEra", "gapPrevious"
      )
    
    selectRowsCombinationWindow(andromeda)
    
    iterations <- iterations + 1
  }
  
  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  message(sprintf("After combinationWindow: %s", nRows))
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    select(-"gapPrevious", -"selectedRows")
  
  time2 <- Sys.time()
  message(sprintf(
    "Time needed to execute combination window %s",
    difftime(time2, time1, units = 'mins')
  ))
  
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
selectRowsCombinationWindow <- function(andromeda) {
  # Order treatmentHistory by person_id, event_start_date, event_end_date
  # andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
  #   arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)
  
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(sortOrder = as.numeric(.data$eventStartDate) + as.numeric(.data$eventEndDate) * row_number() / n() * 10^-6) %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::mutate(gapPrevious = .data$eventStartDate - dplyr::lag(.data$eventEndDate, order_by = .data$sortOrder)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(allRows = ifelse(.data$gapPrevious < 0, dplyr::row_number(), NA)) %>%
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
  
  treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(selectedRows = dplyr::case_when(
      dplyr::row_number() %in% rows ~ 1,
      .default = 0
    ))
  
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
    dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)
  
  if (filterTreatments != "All") {
    message("Order the combinations.")
    
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
  
  if (filterTreatments == "First") {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId, .data$eventCohortId) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  } else if (filterTreatments == "Changes") {
    # Group all rows per person for which previous treatment is same
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::collect() %>%
      dplyr::mutate(group = dplyr::consecutive_id(.data$personId, .data$eventCohortId))
    
    # Remove all rows with same sequential treatments
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId, .data$age, .data$sex, .data$indexYear, .data$eventCohortId, .data$group, .data$sortOrder) %>%
      dplyr::summarise(
        eventStartDate = min(.data$eventStartDate, na.rm = TRUE),
        eventEndDate = max(.data$eventEndDate, na.rm = TRUE),
        durationEra = sum(.data$durationEra, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$personId, .data$indexYear, .data$group) %>%
      dplyr::select(-"group")
  }
  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  message(sprintf("After filterTreatments: %s", nRows))
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
