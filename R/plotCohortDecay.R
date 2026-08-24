collapse_sum <- function(...) {
  vecs <- list(...)
  n <- max(lengths(vecs))
  vecs |>
    purrr::map(\(x) c(x, rep(0, n - length(x)))) |>
    purrr::reduce(`+`)
}

fetchDecayData <- function(cohortTable, censorDate) {
  if (censorDate == "observation_end") {
    x <- cohortTable |>
      dplyr::left_join(cdm$observation_period, by = dplyr::join_by(subject_id == person_id)) |>
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", censor = "observation_period_end_date")
  } else if (censorDate == "cohort_end_date") {
    x <- cohortTable |>
      dplyr::mutate(censor = .data$cohort_end_date) |>
      dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "censor")
  }
  x <- x |>
    dplyr::mutate(
      start = 0,
      end = .data$censor - .data$cohort_start_date,
    ) |>
    dplyr::select("cohort_definition_id", "start", "end") |>
    dplyr::collect() |>
    dplyr::rowwise() |>
    dplyr::mutate(seq = list(rep(1L, end))) |>
    dplyr::pull(.data$seq) |>
    do.call(what = collapse_sum)
  
  dat <- data.frame(
    count = x,
    day = 1:length(x)
  )
  
  dat |>
    dplyr::slice_min(order_by = .data$day, by = "count")
}

#' plotCohortDecay
#' 
#' Plots the count of persons on a given day for each day in a cohort, until a given censor date
#'
#' @param cdm (`cdm_reference`) CDM reference object
#' @param cohorts (`data.frame`) data.frame containing `cohortId`, `cohortName`, and `type` columns
#' @param cohortTableName (`character(1)`) Name of the cohort table in the CDM reference object
#' @param censorDate (`character(1)`: `"observation_end"`) Either one of `"observation_end"` or `"cohort_end_date"` 
#' @param type (`character(n)`) Which cohort `type` should be considered. Whatever type is listed in the `cohorts` argument; usually `"target"` and / or `"event"`
#' @param facet Character or formula to indicate how to facet the plot
#' @param colour (`character(1)`: `"cohort"`) Either `"cohort"` or `NULL`.
#'
#' @returns `ggplot`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
#'   cdm <- CDMConnector::cdmFromCon(con, "main", "main")
#'   cohortSet <- CDMConnector::readCohortSet("./inst/exampleCohorts/")
#'   cdm <- CDMConnector::generateCohortSet(cdm, cohortSet, name = "cohort_table")
#'   
#'   cohorts <- cohortSet |>
#'     dplyr::select(
#'       cohortId = "cohort_definition_id",
#'       cohortName = "cohort_name",
#'     ) |>
#'     dplyr::mutate(
#'       type = c(rep("event", 7), "target")
#'     )
#'
#'   plotCohortDecay(
#'     cdm = cdm,
#'     cohorts = cohorts,
#'     cohortTableName = "cohort_table"
#'   )
#' }
#' }
plotCohortDecay <- function(
    cdm,
    cohorts,
    cohortTableName,
    censorDate = "observation_end",
    type = "target",
    facet = "cohort",
    colour = "cohort"
  ) {
  cohortIds <- cohorts |>
    dplyr::filter(.data$type %in% !!type) |>
    dplyr::pull(.data$cohortId)

  cohortNames <- cohorts |>
    dplyr::filter(.data$cohortId %in% cohortIds) |>
    dplyr::pull(.data$cohortName)

  dat <- cohortIds |>
    purrr::map(\(id) dplyr::filter(cdm[[cohortTableName]], .data$cohort_definition_id == id)) |>
    purrr::map(fetchDecayData, censor = censorDate) |>
    purrr::map2(.y = cohortNames, \(x, y) dplyr::mutate(x, cohort = y)) |>
    purrr::reduce(dplyr::bind_rows)

  gg <- if (is.null(colour)) {
    ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = .data$day, y = .data$count, group = .data$cohort))
  } else {
    ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = .data$day, y = .data$count, group = .data$cohort, colour = .data[[colour]]))
  }

  gg +
    ggplot2::geom_step() +
    ggplot2::facet_grid(facet) +
    visOmopResults::themeVisOmop(style = "darwin")
}
