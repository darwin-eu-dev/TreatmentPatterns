#' summariseOnEvent
#'
#' Summarises the counts of persons that are on an event during the Target cohort per time increment.
#'
#' @param andromeda (`Andromeda`) Andromeda object returned by `computePathways()`
#' @param minCellCount (`numeric(1)`) Minimum cell count. Censors any count below it to `NA`.
#' @param timeScale (`character(1)`: `"day"`) Sets the time scale to group the counts on. Also used as a label when `timeGroup` is set.
#' @param timeGroup (`numeric(1)`: `NULL`) Sets the time group associated with the `timeScale`.
#'
#' @returns `data.frame`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseOnEvent(outputEnv, minCellCount = 5)
#'
#'   # Groups on quarters (90 days)
#'   result <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter" , timeGroup = 90)
#' }
#' }
summariseOnEvent <- function(andromeda, minCellCount, timeScale = "day", timeGroup = NULL) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(andromeda$treatmentHistoryFinal, "tbl_Andromeda", add = assertions)
  checkmate::assertClass(andromeda$cohortTable, "tbl_Andromeda", add = assertions)
  checkmate::assertClass(andromeda$analyses, "tbl_Andromeda", add = assertions)
  checkmate::assertClass(andromeda$cohorts, "tbl_Andromeda", add = assertions)
  checkmate::assertIntegerish(minCellCount, lower = 1, len = 1, null.ok = TRUE, add = assertions)
  checkmate::assertCharacter(timeScale, len = 1, null.ok = FALSE, add = assertions)
  checkmate::assertIntegerish(timeGroup, lower = 0, len = 1, null.ok = TRUE, add = assertions)
  checkmate::reportAssertions(assertions)

  analysisId <- andromeda$analyses |>
    dplyr::pull(.data$analysis_id)

  timeScaleMap <- list(
    day = 1,
    week = 7,
    month = 30,
    year = 365
  )

  if (timeScale %in% names(timeScaleMap) & !is.null(timeGroup)) {
    warning(
      sprintf(
        "`timeGroup` is specified, and is overriding `timeScale`: %s.\n\nTo avoid this warning specify another `timeScale` other than: 'day', 'week', 'month', or 'year'.",
        timeScale
      )
    )
  }

  if (is.null(timeGroup) & !timeScale %in% names(timeScaleMap)) {
    stop("`timeGroup` is not specified, and `timeScale` (%s) is not one of: 'day', 'week', 'month', or 'year',")
  }

  if (!is.null(timeGroup)) {
    timeScaleMap[[timeScale]] <- timeGroup
  }

  totalPop <- andromeda$cohortTable |>
    dplyr::filter(.data$type == "target") |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::pull(.data$n)

  df <- andromeda$treatmentHistoryFinal |>
    dplyr::left_join(andromeda$cohortTable, by = dplyr::join_by(personId == personId, targetCohortId == cohortId)) |>
    dplyr::left_join(andromeda$cohorts, by = dplyr::join_by(targetCohortId == cohortId)) |>
    dplyr::select(
      "personId",
      "cohortName",
      "eventStartDate",
      "eventEndDate",
      targetStartDate = "startDate",
      targetEndDate = "endDate"
    ) |>
    dplyr::mutate(
      s1 = as.integer(abs(.data$eventStartDate) - 1),
      s2 = as.integer(.data$eventEndDate - .data$eventStartDate),
      s3 = as.integer((.data$targetEndDate + 1) - .data$eventEndDate)
    ) |>
    dplyr::mutate(
      l = dplyr::sql("list_concat(repeat([0], s1), repeat([1], s2), repeat([0], s3))")
    ) |>
    dplyr::mutate(
      pos = dplyr::sql("generate_subscripts(l, 1)"),
      value = dplyr::sql("unnest(l)")
    ) |>
    dplyr::select("personId", "cohortName", "pos", "value") |>
    dplyr::group_by(.data$cohortName) |>
    dplyr::mutate(
      pos = dplyr::sql(
        sprintf("FLOOR(pos / %s) + 1::INT", timeScaleMap[[timeScale]])
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      value = max(value, na.rm = TRUE),
      .by = c("pos", "cohortName", "personId")
    ) |>
    dplyr::summarise(
      value = sum(.data$value),
      .by = c("pos", "cohortName")
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      analysis_id = !!analysisId,
      timeScale = !!timeScale,
      pct = .data$value / !!totalPop * 100
    ) |>
    dplyr::rename(
      target_cohort = "cohortName"
    ) |>
    dplyr::arrange(.data$pos)

  df |>
    dplyr::mutate(
      value = dplyr::case_when(
        .data$value < minCellCount & .data$value > 0 ~ NA,
        .default = .data$value
      ),
      pct = dplyr::case_when(
        is.na(.data$value) ~ NA,
        .default = .data$pct
      )
    ) |>
    dplyr::rename(n = "value")
}

#' plotOnEvent
#'
#' Plots the counts of persons that are on an event during the Target cohort per time increment.
#'
#' @param result (`data.frame`) The result of `summariseOnEvent()`
#' @param ... Arguments for `visOmopResults::themeVisOmop()`
#'
#' @returns `ggplot`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseOnEvent(outputEnv, minCellCount = 5)
#'
#'   plotOnEvent(result)
#'
#'   # Groups on quarters (90 days)
#'   result <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter" , timeGroup = 90)
#'   plotOnEvent(result, style = "darwin")
#' }
#' }
plotOnEvent <- function(result, ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertNames(names(result), identical.to = c("pos", "target_cohort", "n", "analysis_id", "timeScale", "pct"), add = assertions)
  checkmate::reportAssertions(assertions)

  ggplot2::ggplot(
    data = result |>
      dplyr::mutate(n = dplyr::case_when(
        is.na(.data$n) ~ 0,
        .default = .data$n
      )),
    mapping = ggplot2::aes(x = .data$pos, y = .data$n)
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = unique(result$timeScale),
      y = "Number of subjects"
    ) +
    ggplot2::facet_grid(rows = ggplot2::vars(target_cohort)) +
    visOmopResults::themeVisOmop(...)
}

#' tableOnEvent
#'
#' Tables the counts of persons that are on an event during the Target cohort per time increment.
#'
#' @param result (`data.frame`) The result of `summariseOnEvent()`
#' @param .force (`logical(1)`) A check is implemented to make sure you really want to return a table of > 1000 rows (usually no). `.force` circumvents this check.
#' @param ... Arguments for `visOmopResults::visTable()`
#'
#' @returns A formatted table of the class selected in "type" argument.
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseOnEvent(outputEnv, minCellCount = 5)
#'
#'   tableOnEvent(result)
#'
#'   # Groups on quarters (90 days)
#'   result <- summariseOnEvent(outputEnv, minCellCount = 5, timeScale = "quarter" , timeGroup = 90)
#'   tableOnEvent(result, style = "darwin")
#' }
#' }
tableOnEvent <- function(result, .force = FALSE, ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertNames(names(result), identical.to = c("pos", "target_cohort", "n", "analysis_id", "timeScale", "pct"), add = assertions)
  checkmate::assertLogical(.force, len = 1, add = assertions)
  checkmate::reportAssertions(assertions)

  toTable <- FALSE

  if (isTRUE(.force)) {
    toTable <- TRUE
  }

  if (nrow(result) > 1000) {
    res <- readline(message("`result` contains > 1000 rows to display in the table, are you sure? (y/n)"))
    if (res == "y") {
      toTable <- TRUE
    } 
    message("To disable this check set `.force = TRUE`")
  } else {
    toTable <- TRUE
  }

  if (toTable) {
    result |>
      dplyr::relocate("analysis_id", "target_cohort") |>
      dplyr::rename(
        !!rlang::sym(unique(result$timeScale)) := "pos"
      ) |>
      dplyr::mutate(pct = round(.data$pct, 2)) |>
      dplyr::select(-"timeScale", `%` = "pct") |>
      visOmopResults::visTable(...)
  }
}
