summariseTargetIndexToEndOfObservation <- function(andromeda) {
  andromeda$cohortTable |>
    dplyr::filter(.data$type == "target") |>
    dplyr::mutate(
      observation_period_start_date = .data$observation_period_start_date - as.Date("1970-01-01"),
      observation_period_end_date = .data$observation_period_end_date - as.Date("1970-01-01")
    ) |>
    dplyr::mutate(
      diff = .data$observation_period_end_date - .data$startDate
    ) |>
    dplyr::summarise(
      min = min(.data$diff, na.rm = TRUE),
      q25 = stats::quantile(.data$diff, 0.25, na.rm = TRUE),
      median = stats::median(.data$diff, na.rm = TRUE),
      q75 = stats::quantile(.data$diff, 0.75, na.rm = TRUE),
      max = max(.data$diff, na.rm = TRUE),
      mean = mean(.data$diff, na.rm = TRUE),
      sd = stats::sd(.data$diff, na.rm = TRUE),
      .by = c("cohortId")
    ) |>
    dplyr::collect()
}

summariseTargetIndexToEndOfObservation(outputEnv)
