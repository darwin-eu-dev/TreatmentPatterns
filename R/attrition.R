fetchAttritionCounts <- function(andromeda, table = "treatmentHistory") {
  n <- andromeda[[table]] %>%
    dplyr::group_by(.data$personId) %>% 
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull()
  list(
    nSubjects = length(n),
    nRecords = sum(n)
  )
}

initAttrition <- function(andromeda) {
  andromeda$attrition <- data.frame(
    number_records = numeric(0),
    number_subjects = numeric(0),
    reason_id = numeric(0),
    reason = character(0),
    time_stamp = numeric(0)
  )
}

appendAttrition <- function(toAdd, andromeda) {
  if (is.null(andromeda$attrition)) initAttrition(andromeda)
  
  toAdd$time_stamp <- as.numeric(Sys.time())
  
  andromeda$attrition <- dplyr::rows_append(
    x = andromeda$attrition,
    y = toAdd,
    copy = TRUE
  )

  message(sprintf("-- %s\n\tRecords: %s\n\tSubjects: %s", toAdd$reason, toAdd$number_records, toAdd$number_subjects))
}
