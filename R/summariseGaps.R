#' summariseGaps
#'
#' Summarises the gaps: 1) Between Target cohort entry and the first Event; 2)
#' Between events; and 3) Between the last Event's end and the Target exit.
#'
#' @param andromeda (`Andromeda`) Andromeda object returned by `computePathways()` 
#'
#' @returns `data.frame`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseGaps(outputEnv)
#'
#'   plotGaps(result, timeScale = "week", style = "darwin")
#'   tableGaps(result)
#' }
#' }
summariseGaps <- function(andromeda) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(andromeda$treatmentHistoryFinal, "tbl_Andromeda", add = assertions)
  checkmate::assertClass(andromeda$analyses, "tbl_Andromeda", add = assertions)
  checkmate::assertClass(andromeda$cohorts, "tbl_Andromeda", add = assertions)
  checkmate::reportAssertions(assertions)

  analysisId <- andromeda$analyses |>
    dplyr::pull(.data$analysis_id)

  df <- andromeda$treatmentHistoryFinal |>
    dplyr::left_join(
      andromeda$cohortTable,
      by = dplyr::join_by(personId == personId, targetCohortId == cohortId)
    ) |>
    dplyr::left_join(
      andromeda$cohorts,
      by = dplyr::join_by(targetCohortId == cohortId, type == type)
    ) |>
    dplyr::select(
      "eventCohortId", "cohortName", "personId",
      "eventStartDate", "eventEndDate",
      targetStartDate = "startDate",
      targetEndDate = "endDate",
      "eventSeq",
      age = "age.x", sex = "sex.x", "indexYear"
    ) |>
    dplyr::group_by(.data$personId, .data$cohortName) |>
    dbplyr::window_order(.data$eventSeq) |>
    dplyr::mutate(
      gap_to_first = dplyr::case_when(
        .data$eventSeq == min(.data$eventSeq, na.rm = TRUE)
        ~ .data$eventStartDate - .data$targetStartDate
      ),
      gap_to_end = dplyr::case_when(
        .data$eventSeq == max(.data$eventSeq, na.rm = TRUE)
        ~ .data$targetEndDate - .data$eventEndDate
      ),
      gap_between = dplyr::lead(.data$eventStartDate) - .data$eventEndDate
    ) |>
    dplyr::ungroup() |>
    dbplyr::window_order()

  layers <- df |>
    dplyr::summarise(
      layer = .data$eventSeq,
      analysis_id = !!analysisId,
      type = dplyr::sql("'gap_layer_' || eventSeq::INT || '-' || (eventSeq + 1)::INT"),
      min = min(.data$gap_between, na.rm = TRUE),
      q25 = stats::quantile(.data$gap_between, 0.25, na.rm = TRUE),
      median = stats::median(.data$gap_between, na.rm = TRUE),
      q75 = stats::quantile(.data$gap_between, 0.75, na.rm = TRUE),
      max = max(.data$gap_between, na.rm = TRUE),
      mean = mean(.data$gap_between, na.rm = TRUE),
      sd = stats::sd(.data$gap_between, na.rm = TRUE),
      .by = c("cohortName", "eventSeq")
    ) |>
    dplyr::filter(.data$layer != max(.data$layer))

  ends <- c("gap_to_first", "gap_to_end") |>
    purrr::map(\(col) {
      df |>
        dplyr::summarise(
          analysis_id = !!analysisId,
          type = !!col,
          min = min(.data[[col]], na.rm = TRUE),
          q25 = stats::quantile(.data[[col]], 0.25, na.rm = TRUE),
          median = stats::median(.data[[col]], na.rm = TRUE),
          q75 = stats::quantile(.data[[col]], 0.75, na.rm = TRUE),
          max = max(.data[[col]], na.rm = TRUE),
          mean = mean(.data[[col]], na.rm = TRUE),
          sd = stats::sd(.data[[col]], na.rm = TRUE),
          .by = c("cohortName")
        )
    }) |>
    purrr::reduce(dplyr::union_all)

  layers |>
    dplyr::union_all(ends) |>
    dplyr::select(
      "analysis_id",
      target_cohort = "cohortName",
      "type",
      "min",
      "q25",
      "median",
      "q75",
      "max",
      "mean",
      "sd"
    ) |>
    dplyr::collect()
}

#' plotGaps
#'
#' Plots the gaps: 1) Between Target cohort entry and the first Event; 2)
#' Between events; and 3) Between the last Event's end and the Target exit.
#'
#' @param result (`data.frame`) Result from `summariseGaps()`
#' @param timeScale (`character(1)`: `"day"`) Timescale to use as the x-axis in
#' the plot. May be one of: `"day"`, `"week"`, `"month"`, `"year"`
#' @param ... Arguments for `visOmopResults::themeVisOmop()`
#'
#' @returns `ggplot`
#' @export
#'
#' @examples {
#' if (interactive()) {
#'   result <- summariseGaps(outputEnv)
#'
#'   plotGaps(result, timeScale = "week", style = "darwin")
#'   tableGaps(result)
#' }
#' }
plotGaps <- function(result, timeScale = "day", ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(result, "data.frame", add = assertions)
  checkmate::assertNames(
    x = names(result),
    identical.to = c(
      "analysis_id", "target_cohort", "type", "min", "q25", "median", "q75",
      "max","mean", "sd"
    ),
    add = assertions
  )
  checkmate::assertChoice(
    timeScale,
    choices = c("day", "week", "month", "year"),
    add = assertions
  )
  checkmate::reportAssertions(assertions)

  scaleMap <- list(
    year = 365.25,
    month = 30,
    week = 7,
    day = 1
  )
  
  df <- result |>
    dplyr::mutate(
      min = .data$min / !!scaleMap[[timeScale]],
      q25 = .data$q25 / !!scaleMap[[timeScale]],
      median = .data$median / !!scaleMap[[timeScale]],
      q75 = .data$q75 / !!scaleMap[[timeScale]],
      max = .data$max / !!scaleMap[[timeScale]],
      mean = .data$mean / !!scaleMap[[timeScale]],
      sd = .data$sd / !!scaleMap[[timeScale]],
      type = dplyr::case_when(
        .data$type == "gap_to_first" ~ "time to first event",
        .data$type == "gap_to_end" ~ "time from last event to end of target",
        .data$type == "gap_between" ~ "time between events"
      )
    )

  ggplot2::ggplot(data = df, mapping = ggplot2::aes(group = .data$type)) +
    ggplot2::geom_boxplot(
      stat = "identity",
      mapping = ggplot2::aes(
        y = .data$type,
        xmin = .data$min,
        xlower = .data$q25,
        xmiddle = .data$median,
        xupper = .data$q75,
        xmax = .data$max
      )
    ) +
    ggplot2::facet_grid(rows = ggplot2::vars(target_cohort)) +
    visOmopResults::themeVisOmop(...) +
    ggplot2::theme(axis.title.y.left = ggplot2::element_blank()) +
    ggplot2::labs(
      x = sprintf("time (%s)", timeScale)
    )
}

#' tableGaps
#'
#' Creates a table of summary statistics of the gaps: 1) Between Target cohort
#' entry and the first Event; 2) Between events; and 3) Between the last
#' Event's end and the Target exit.
#'
#' @param result (`data.frame`) Result from `summariseGaps()`
#' @param timeScale (`character(1)`: `"day"`) Time scale to use for the summary
#' statistics. May be one of: `"day"`, `"week"`, `"month"`, `"year"`
#' @param ... Arguments for `visOmopResults::visTable()`
#'
#' @returns `data.frame`
#' @export
#' 
#' @examples {
#' if (interactive()) {
#'   result <- summariseGaps(outputEnv)
#'
#'   plotGaps(result, timeScale = "week", style = "darwin")
#'   tableGaps(result)
#' }
#' }
tableGaps <- function(result, timeScale = "day", ...) {
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertClass(result, "data.frame", add = assertions)
  checkmate::assertNames(
    x = names(result),
    identical.to = c(
      "analysis_id", "target_cohort", "type", "min", "q25", "median", "q75",
      "max", "mean", "sd"
    ),
    add = assertions
  )
  checkmate::assertChoice(
    timeScale,
    choices = c("day", "week", "month", "year"),
    add = assertions
  )
  checkmate::reportAssertions(assertions)

  scaleMap <- list(
    year = 365.25,
    month = 30,
    week = 7,
    day = 1
  )
  
  result |>
    dplyr::mutate(
      min = round(.data$min / !!scaleMap[[timeScale]], 2),
      q25 = round(.data$q25 / !!scaleMap[[timeScale]], 2),
      median = round(.data$median / !!scaleMap[[timeScale]], 2),
      q75 = round(.data$q75 / !!scaleMap[[timeScale]], 2),
      max = round(.data$max / !!scaleMap[[timeScale]], 2),
      mean = round(.data$mean / !!scaleMap[[timeScale]], 2),
      sd = round(.data$sd / !!scaleMap[[timeScale]], 2),
      time_scale = !!timeScale,
      type = dplyr::case_when(
        .data$type == "gap_to_first" ~ "Time to first event",
        .data$type == "gap_to_end" ~ "Time from last event to end of target",
        .data$type == "gap_between" ~ "Time between events"
      )
    ) |>
    visOmopResults::visTable(...)
}
