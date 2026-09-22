#' summariseTargetCohortDecay
#'
#' Summarises the decay of the Target cohort over time.
#'
#' @param andromeda (`Andromeda`) Andromeda object returned by `computePathways()` 
#' @param minCellCount (`numeric(1)`) Cut off value to censor any counts, that fall below this value.
#'
#' @returns `data.frame`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseTargetCohortDecay(outputEnv)
#'
#'   plotTargetCohortDecay(result, timeScale = "week", style = "darwin")
#'   tableTargetCohortDecay(result)
#'   tableTargetCohortDecayAtDays(result, timePoints = 1:30, timeScale = "year", style = "darwin")
#' }
#' }
summariseTargetCohortDecay <- function(andromeda, minCellCount) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(andromeda$cohortTable, "tbl_Andromeda", add = assertions)
  checkmate::assertClass(andromeda$analyses, "tbl_Andromeda", add = assertions)
  checkmate::assertClass(andromeda$cohorts, "tbl_Andromeda", add = assertions)
  checkmate::assertIntegerish(minCellCount, lower = 1, len = 1, null.ok = TRUE, add = assertions)
  checkmate::reportAssertions(assertions)

  analysisId <- andromeda$analyses |>
    dplyr::pull(.data$analysis_id)

  totalPop <- andromeda$cohortTable |>
    dplyr::filter(.data$type == "target") |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull(.data$n)

  andromeda$cohortTable |>
    dplyr::left_join(andromeda$cohorts, by = c("cohortId", "type")) |>
    dplyr::filter(.data$type == "target") |>
    dplyr::mutate(
      indexYear = floor(.data$startDate / 365.25 + 1970),
      observation_period_start_date = .data$observation_period_start_date - as.Date("1970-01-01"),
      observation_period_end_date = .data$observation_period_end_date - as.Date("1970-01-01")
    ) |>
    dplyr::mutate(
      start_point = .data$startDate - .data$startDate,
      end_point = abs(.data$startDate) + abs(.data$observation_period_end_date)
    ) |>
    dplyr::select("cohort_name", "personId", "start_point", "end_point") |>
    dplyr::mutate(
      l = dplyr::sql("repeat([1], end_point)")
    ) |>
    dplyr::mutate(
      pos = dplyr::sql("generate_subscripts(l, 1)"),
      value = dplyr::sql("unnest(l)")
    ) |>
    dplyr::summarise(
      value = sum(.data$value),
      .by = c("pos", "cohort_name")
    ) |>
    dplyr::group_by(.data$value) |>
    dplyr::filter(
      .data$pos == min(.data$pos)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$pos) |>
    dplyr::collect() |>
    dplyr::mutate(
      analysis_id = analysisId,
      pct = .data$value / !!totalPop * 100
    ) |>
    dplyr::select(
      "analysis_id",
      target_cohort = "cohort_name",
      n = "value",
      "pct",
      days = "pos"
    ) |>
    dplyr::mutate(
      n = dplyr::case_when(
        .data$n < !!minCellCount ~ NA,
        .default = .data$n
      ),
      pct = dplyr::case_when(
        is.na(.data$n) ~ NA,
        .default = .data$pct
      )
    )
}

#' plotTargetCohortDecay
#'
#' Plots the decay of the target cohort over time.
#'
#' @param result (`data.frame`) Result from `summariseTargetCohortDecay()`
#' @param timeScale (`character(1)`: `"day"`) Timescale to use as the x-axis in the plot. May be one of: `"day"`, `"week"`, `"month"`, `"year"`
#' @param ... Arguments for `visOmopResults::themeVisOmop()`
#'
#' @returns `ggplot`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseTargetCohortDecay(outputEnv)
#'
#'   plotTargetCohortDecay(result, timeScale = "week", style = "darwin")
#'   tableTargetCohortDecay(result)
#'   tableTargetCohortDecayAtDays(result, timePoints = 1:30, timeScale = "year", style = "darwin")
#' }
#' }
plotTargetCohortDecay <- function(result, timeScale = "day", ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(result, "data.frame", add = assertions)
  checkmate::assertNames(names(result), identical.to = c("analysis_id", "target_cohort", "n", "pct", "days"), add = assertions)
  checkmate::assertChoice(timeScale, choices = c("day", "week", "month", "year"), add = assertions)
  checkmate::reportAssertions(assertions)

  df <- result |>
    dplyr::mutate(
      time = dplyr::case_when(
        !!timeScale == "year" ~ .data$days / 365.25,
        !!timeScale == "month" ~ .data$days / 30,
        !!timeScale == "week" ~ .data$days / 7,
        .default = .data$days
      ),
      n = dplyr::case_when(
        is.na(.data$n) ~ 0,
        .default = .data$n
      ),
      pct = dplyr::case_when(
        is.na(.data$pct) ~ 0,
        .default = .data$pct
      )
    )

  ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = .data$time,
      y = .data$pct,
      group = .data$target_cohort
    )) +
    ggplot2::geom_step() +
    ggplot2::facet_grid(rows = ggplot2::vars(target_cohort)) +
    ggplot2::labs(
      title = "Target Cohort Decay",
      y = "%",
      x = sprintf("time (%s)", timeScale)
    ) +
    visOmopResults::themeVisOmop(...)
}

#' tableTargetCohortDecay
#'
#' Creates a table of summary statistics of the cohort decay.
#'
#' @param result (`data.frame`) Result from `summariseTargetCohortDecay()`
#' @param timeScale (`character(1)`: `"day"`) Time scale to use for the summary statistics. May be one of: `"day"`, `"week"`, `"month"`, `"year"`
#' @param ... Arguments for `visOmopResults::visTable()`
#'
#' @returns A formatted table of the class selected in "type" argument.
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseTargetCohortDecay(outputEnv)
#'
#'   plotTargetCohortDecay(result, timeScale = "week", style = "darwin")
#'   tableTargetCohortDecay(result)
#'   tableTargetCohortDecayAtDays(result, timePoints = 1:30, timeScale = "year", style = "darwin")
#' }
#' }
tableTargetCohortDecay <- function(result, timeScale = "day", ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(result, "data.frame", add = assertions)
  checkmate::assertNames(names(result), identical.to = c("analysis_id", "target_cohort", "n", "pct", "days"), add = assertions)
  checkmate::assertChoice(timeScale, choices = c("day", "week", "month", "year"), add = assertions)
  checkmate::reportAssertions(assertions)

  result |>
    dplyr::mutate(
      time = dplyr::case_when(
        !!timeScale == "year" ~ .data$days / 365.25,
        !!timeScale == "month" ~ .data$days / 30,
        !!timeScale == "week" ~ .data$days / 7,
        .default = .data$days
      )
    ) |>
    dplyr::summarise(
      min = round(min(.data$time, na.rm = TRUE), 2),
      q25 = round(stats::quantile(.data$time, 0.25, na.rm = TRUE), 2),
      median = round(stats::median(.data$time, na.rm = TRUE), 2),
      q75 = round(stats::quantile(.data$time, 0.75, na.rm = TRUE), 2),
      max = round(max(.data$time, na.rm = TRUE), 2),
      mean = round(mean(.data$time, na.rm = TRUE), 2),
      sd = round(stats::sd(.data$time, na.rm = TRUE), 2),
      .by = c("analysis_id", "target_cohort")
    ) |>
    dplyr::mutate(time = timeScale) |>
    dplyr::relocate("analysis_id", "target_cohort", "time") |>
    visOmopResults::visTable(...)
}

#' tableTargetCohortDecayAtDays
#'
#' Creates a table of the number of subjects in the Target cohort, at given days, weeks, months or years.
#'
#' @param result (`data.frame`) Result from `summariseTargetCohortDecay()`
#' @param timePoints (`numeric(n)`) A vector of time points.
#' @param timeScale (`character(1)`: `"day"`) Time scale to use for the summary statistics. May be one of: `"day"`, `"week"`, `"month"`, `"year"`
#' @param ... Arguments for `visOmopResults::visTable()`
#'
#' @returns `data.frame`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseTargetCohortDecay(outputEnv)
#'
#'   plotTargetCohortDecay(result, timeScale = "week", style = "darwin")
#'   tableTargetCohortDecay(result)
#'   tableTargetCohortDecayAtDays(result, timePoints = 1:30, timeScale = "year", style = "darwin")
#' }
#' }
tableTargetCohortDecayAtDays <- function(result, timePoints, timeScale = "day", ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(result, "data.frame", add = assertions)
  checkmate::assertNames(names(result), identical.to = c("analysis_id", "target_cohort", "n", "pct", "days"), add = assertions)
  checkmate::assertIntegerish(timePoints, lower = 0, min.len = 1, null.ok = FALSE, add = assertions)
  checkmate::assertChoice(timeScale, choices = c("day", "week", "month", "year"), add = assertions)
  checkmate::reportAssertions(assertions)

  timePoints |>
    purrr::map(\(timePoint) {
      result |>
        dplyr::mutate(
          time = dplyr::case_when(
            !!timeScale == "year" ~ .data$days / 365.25,
            !!timeScale == "month" ~ .data$days / 30,
            !!timeScale == "week" ~ .data$days / 7,
            .default = .data$days
          )
        ) |>
        dplyr::filter(.data$time <= timePoint) |>
        dplyr::filter(.data$time == max(.data$time, na.rm = TRUE)) |>
        dplyr::mutate(
          pct = round(.data$pct, 2),
          !!rlang::sym(timeScale) := timePoint
        ) |>
        dplyr::select("analysis_id", "target_cohort", dplyr::any_of(timeScale), "n", "pct")
    }) |>
    purrr::reduce(dplyr::bind_rows) |>
    visOmopResults::visTable(...)
}
