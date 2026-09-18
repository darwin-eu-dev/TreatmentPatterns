#' summariseTargetIndexToEndOfObservation
#'
#' Computes summary statistics of the duration from the index date of the
#' Target cohort to the end of observation.
#'
#' @param andromeda (`Andromeda`) Andromeda object returned by `computePathways()`
#' @param minCellCount (`numeric(1)`) Any count values will be cencored below this value.
#'
#' @returns `data.frame`
#' @export
#'
#' @examples {
#'   if (interactive()) {
#'     outputEnv <- computePathways(
#'       cohorts = cohorts,
#'       cohortTableName = "cohort_table",
#'       cdm = cdm
#'     )
#'     result <- summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 5)
#'   }
#' }
summariseTargetIndexToEndOfObservation <- function(andromeda, minCellCount) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(andromeda, classes = "Andromeda", add = assertions)
  checkmate::assertClass(andromeda$cohortTable, classes = "tbl_Andromeda", .var.name = "cohortTable", add = assertions)
  checkmate::assertClass(andromeda$treatmentHistoryFinal, classes = "tbl_Andromeda", .var.name = "treatmentHistoryFinal", add = assertions)
  checkmate::assertClass(andromeda$analyses, classes = "tbl_Andromeda", .var.name = "analyses", add = assertions)
  checkmate::assertClass(andromeda$cohorts, classes = "tbl_Andromeda", .var.name = "cohorts", add = assertions)
  checkmate::assertIntegerish(x = minCellCount, lower = 0, len = 1, null.ok = FALSE, add = assertions)
  checkmate::reportAssertions(assertions)

  andromeda$indexToEndObs <- andromeda$cohortTable |>
    dplyr::left_join(andromeda$treatmentHistoryFinal) |>
    dplyr::mutate(events = dplyr::case_when(
      is.na(.data$eventCohortId) ~ "valid events",
      .default = "no valid events"
    )) |>
    dplyr::filter(.data$type == "target") |>
    dplyr::mutate(
      indexYear = floor(.data$startDate / 365.25 + 1970),
      observation_period_start_date = .data$observation_period_start_date - as.Date("1970-01-01"),
      observation_period_end_date = .data$observation_period_end_date - as.Date("1970-01-01")
    ) |>
    dplyr::mutate(
      diff = .data$observation_period_end_date - .data$startDate
    )

  analysisId <- andromeda$analyses |>
    dplyr::pull(.data$analysis_id)

  targetCohorts <- andromeda$cohorts |>
    dplyr::filter(.data$type == "target")

  result <- andromeda$indexToEndObs |>
    dplyr::summarise(
      min = min(.data$diff, na.rm = TRUE),
      q25 = stats::quantile(.data$diff, 0.25, na.rm = TRUE),
      median = stats::median(.data$diff, na.rm = TRUE),
      q75 = stats::quantile(.data$diff, 0.75, na.rm = TRUE),
      max = max(.data$diff, na.rm = TRUE),
      mean = mean(.data$diff, na.rm = TRUE),
      sd = stats::sd(.data$diff, na.rm = TRUE),
      n = dplyr::n(),
      .by = c("cohortId", "events")
    ) |>
    dplyr::mutate(
      pct = n / sum(n, na.rm = TRUE) * 100
    ) |>
    dplyr::mutate(
      analysis_id = analysisId
    ) |>
    dplyr::left_join(andromeda$cohorts, by = "cohortId") |>
    dplyr::select(-"type") |>
    dplyr::relocate(
      "analysis_id",
      target_cohort_id = "cohortId",
      target_cohort_name = "cohortName"
    ) |>
    dplyr::collect()

  result |>
    dplyr::mutate(dplyr::across(c("n"), ~ replace(.x, .x < minCellCount, NA))) |>
    dplyr::mutate(dplyr::across(c("pct"), ~ replace(.x, is.na(.data$n), NA)))
}

#' tableTargetIndexToEndOfObservation
#'
#' Makes a table out of the result of `summariseTargetIndexToEndOfObservation()` using `visOmopResults::visTable()`
#'
#' @param result (`data.frame`) Result from `summariseTargetIndexToEndOfObservation()`
#' @param ... Arguments for `visOmopResults::visTable()`
#'
#' @returns A formatted table of the class selected in "type" argument.
#' @export
#'
#' @examples {
#'   if (interactive()) {
#'     outputEnv <- computePathways(
#'       cohorts = cohorts,
#'       cohortTableName = "cohort_table",
#'       cdm = cdm
#'     )
#'     result <- summariseTargetIndexToEndOfObservation(outputEnv, minCellCount = 5)
#'
#'     tableTargetIndexToEndOfObservation(result, style = "darwin")
#'   }
#' }
tableTargetIndexToEndOfObservation <- function(result, ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(result, classes = "data.frame", add = assertions)
  checkmate::assertNames(
    x = names(result),
    must.include = c(
      "analysis_id", "target_cohort_id", "target_cohort_name", "min", "q25",
      "median", "q75", "max", "mean", "sd", "n", "pct"
    )
  )
  checkmate::reportAssertions(assertions)

  result |>
    dplyr::mutate(
      min = round(.data$min, 2),
      q25 = round(.data$q25, 2),
      median = round(.data$median, 2),
      q75 = round(.data$q75, 2),
      max = round(.data$max, 2),
      mean = round(.data$mean, 2),
      sd = round(.data$sd, 2),
      n = round(.data$n, 2),
      pct = round(.data$pct, 2)
    ) |>
    dplyr::rename(
      `%` = "pct"
    ) |>
    visOmopResults::visTable(...)
}
