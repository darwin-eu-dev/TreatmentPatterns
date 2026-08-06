appendAttrition <- function(tbl, andromeda, reason, reason_id) {
  if ("list" %in% class(tbl)) {
    tbl <- tbl |>
      purrr::reduce(dplyr::union_all)
  }
  
  attrition <- tbl |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dplyr::summarise(
      number_records = dplyr::n(),
      number_subjects = dplyr::n_distinct(.data$subject_id)
    ) |>
    dplyr::mutate(
      reason_id = reason_id,
      reason = reason,
      excluded_records = NA,
      excluded_subjects = NA
    ) |>
    dplyr::collect()
  
  andromeda$attrition <- andromeda$attrition |>
    dplyr::union_all(attrition, copy = TRUE) |>
    dplyr::group_by(.data$cohort_definition_id) |>
    dbplyr::window_order(dplyr::desc(.data$number_records)) |>
    dplyr::mutate(
      excluded_records = dplyr::case_when(
        is.na(.data$excluded_records) ~ dplyr::lag(.data$number_records) - .data$number_records
      ),
      excluded_subjects = dplyr::case_when(
        is.na(.data$excluded_subjects) ~ dplyr::lag(.data$number_subjects) - .data$number_subjects
      )
    ) |>
    dplyr::mutate(
      excluded_records = dplyr::case_when(
        is.na(.data$excluded_records) ~ 0,
        .default = .data$excluded_records
      ),
      excluded_subjects = dplyr::case_when(
        is.na(.data$excluded_subjects) ~ 0,
        .default = .data$excluded_subjects
      )
    )
  
  return(andromeda)
}

initAttrition <- function(andromeda) {
  andromeda$attrition <- data.frame(
    cohort_definition_id = integer(0),
    number_records = integer(0),
    number_subjects = integer(0),
    reason_id = integer(0),
    reason = character(0),
    excluded_records = integer(0),
    excluded_subjects = integer(0)
  )
  return(andromeda)
}
