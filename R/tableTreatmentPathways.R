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
    dplyr::mutate(`%` = round(.data$freq / sum(.data$freq) * 100, 2)) |>
    dplyr::ungroup() |>
    dplyr::select(data_source = "cdm_source_abbreviation", analysis = "description", "age", "sex", "index_year", "pathway", n = "freq", "%") |>
    visOmopResults::visTable(...)
}
