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

#' tableTreatmentPathways
#'
#' Function that creates a formatted table of the Treatment Pathways result using `visOmopResults`.
#'
#' @param result (`TreatmentPatternsResults`) Result object from `TreatmentPatterns::export()`.
#' @param ... Additional arguments are passed to `visOmopResults::visTable()`.
#'
#' @returns A formatted table of the class selected in "type" argument.
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   tableTreatmentPathways(result, style = "darwin")
#' }
#' }
tableTreatmentPathways <- function(result, ...) {
  checkmate::assertClass(result, "TreatmentPatternsResults")

  if (is.null(result$treatment_pathways)) {
    stop("`treatment_pathways` is `NULL`")
  }

  result$treatment_pathways |>
    dplyr::left_join(result$cdm_source_info, by = "analysis_id") |>
    dplyr::left_join(result$analyses, by = "analysis_id") |>
    dplyr::group_by(
      .data$cdm_source_abbreviation,
      .data$age,
      .data$sex,
      .data$index_year,
      .data$analysis_id,
      .data$target_cohort_id,
      .data$target_cohort_name
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      data_source = "cdm_source_abbreviation",
      analysis = "description",
      "age",
      "sex",
      "index_year",
      "pathway",
      n = "freq",
      `%` = "pct"
    ) |>
    visOmopResults::visTable(...)
}
