#' tableSummaryEventDuration
#'
#' Function that creates a formatted table of the Summary Event Duration result using `visOmopResults`.
#'
#' @param result (`TreatmentPatternsResults`) Result object from `TreatmentPatterns::export()`.
#' @param ... Additional arguments are passed to `visOmopResults::visTable()`.
#'
#' @returns A formatted table of the class selected in "type" argument.
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   tableSummaryEventDuration(result, style = "darwin")
#' }
#' }
tableSummaryEventDuration <- function(result, ...) {
  result$summary_event_duration |>
    dplyr::left_join(result$cdm_source_info, by = "analysis_id") |>
    dplyr::left_join(result$analyses, by = "analysis_id") |>
    dplyr::group_by(.data$cdm_source_abbreviation, .data$description, .data$line) |>
    dplyr::mutate(
      `%` = round(.data$event_count / sum(.data$event_count) * 100, 2),
      duration_average = round(.data$duration_average, 2),
      duration_sd = round(.data$duration_sd, 2)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      data_source = "cdm_source_abbreviation",
      analysis = "description",
      event_line = "line",
      event = "event_name",
      min = "duration_min",
      Q25 = "duration_q1",
      median = "duration_median",
      Q75 = "duration_q2",
      max = "duration_max",
      mean = "duration_average",
      `st dev` = "duration_sd",
      n = "event_count",
      "%"
    ) |>
    dplyr::arrange(.data$data_source, .data$analysis, .data$event_line) |>
    visOmopResults::visTable(...)
}
