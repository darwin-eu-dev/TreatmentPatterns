# Copyright 2026 DARWIN EU®
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
  checkmate::assertClass(result, classes = "TreatmentPatternsResults")

  if (is.null(result$summary_event_duration)) {
    stop("`summary_event_duration` is `NULL`")
  }

  result$summary_event_duration |>
    dplyr::left_join(result$cdm_source_info, by = "analysis_id") |>
    dplyr::left_join(result$analyses, by = "analysis_id") |>
    dplyr::group_by(.data$cdm_source_abbreviation, .data$description, .data$line) |>
    dplyr::mutate(
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
      `%` = "pct"
    ) |>
    dplyr::arrange(.data$data_source, .data$analysis, .data$event_line) |>
    visOmopResults::visTable(...)
}
