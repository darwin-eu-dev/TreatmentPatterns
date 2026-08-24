addMinEraDuration <- function(studyPopulation, minEraDuration) {
  if (!"min_era_duration_event" %in% colnames(studyPopulation)) {
    studyPopulation |>
      dplyr::mutate(
        min_era_duration_event = minEraDuration,
        min_era_duration_event = minEraDuration
      )
  } else {
    studyPopulation
  }
}

applyMinEraDuration <- function(studyPopulations, minEraDuration) {
  studyPopulations |>
    purrr::map(addMinEraDuration, minEraDuration = minEraDuration) |>
    purrr::map(dplyr::filter, .data$cohort_end_date - .data$cohort_start_date >= .data$min_era_duration_event) |>
    purrr::map(dplyr::filter, .data$target_end_date - .data$target_start_date >= .data$min_era_duration_target)
}

createStudyPopulation <- function(andromeda) {
  andromeda$events <- andromeda$cohort_table |>
    dplyr::filter(.data$type == "event")

  andromeda$targets <- andromeda$cohort_table |>
    dplyr::filter(.data$type == "target")

  targetIds <- andromeda$targets |>
    dplyr::distinct(.data$cohort_definition_id) |>
    dplyr::pull() |>
    as.list()

  targetIds |>
    purrr::map(~ dplyr::filter(andromeda$targets, .data$cohort_definition_id == .x)) |>
    purrr::map(
      dplyr::left_join,
      y = andromeda$events,
      by = dplyr::join_by(
        subject_id == subject_id,
        subject_id_org == subject_id_org,
        x$cohort_start_date <= y$cohort_start_date,
        x$cohort_end_date >= y$cohort_start_date
      ),
      suffix = c("_target", "_event")
    ) |>
    purrr::map(
      dplyr::select, 
      cohort_definition_id = "cohort_definition_id_event",
      "subject_id",
      cohort_start_date = "cohort_start_date_event",
      cohort_end_date = "cohort_end_date_event",
      target_start_date = "cohort_start_date_target",
      target_end_date = "cohort_end_date_target",
      target_id = "cohort_definition_id_target",
      dplyr::matches("min_era_duration"),
      target_name = "cohort_name_target",
      cohort_name = "cohort_name_event",
      "subject_id_org"
    ) |>
    purrr::map2(.y = targetIds, \(pop, id) dplyr::compute(pop, name = sprintf("study_population_%s", id), overwrite = TRUE))
  return(invisible(andromeda))
}

collapseEventEra <- function(studyPopulation, eraCollapseSize) {
  start <- TRUE
  counter <- 0

  tmpStudyPop <- NULL

  while (start) {
    studyPop <- if (is.null(tmpStudyPop)) {
      studyPopulation
    } else {
      tmpStudyPop
    }
    tmpStudyPop <- studyPop |>
      dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
      dbplyr::window_order(.data$cohort_start_date, .data$cohort_end_date) |>
      dplyr::mutate(
        diff = .data$cohort_start_date - dplyr::lag(.data$cohort_end_date),
        flag = dplyr::case_when(
          .data$diff <= eraCollapseSize ~ 1,
          .default = 0
        ),
        flag = dplyr::case_when(
          dplyr::lead(.data$flag) == 1
          | .data$flag == 1
          ~ 1,
          .default = 0
        ),
        row = dplyr::case_when(
          .data$flag == 1 & .data$diff <= eraCollapseSize ~ dplyr::row_number(),
          .default = 0
        ),
        end_date = dplyr::case_when(
          .data$row == max(.data$row, na.rm = TRUE) ~ .data$cohort_end_date
        )
      ) |>
      dplyr::ungroup() |>
      dbplyr::window_order()

    flags <- tmpStudyPop |>
      dplyr::pull(.data$flag) |>
      as.logical()
    
    if (any(flags)) {
      tmpStudyPop <- tmpStudyPop |>
        dplyr::group_by(.data$subject_id, .data$cohort_definition_id) |>
        dbplyr::window_order(.data$cohort_start_date, .data$cohort_end_date) |>
        dplyr::mutate(
          cohort_end_date_old = .data$cohort_end_date,
          cohort_end_date = dplyr::case_when(
            .data$flag == 1 ~ max(.data$end_date, na.rm = TRUE),
            .default = .data$cohort_end_date_old
          )
        ) |>
        dplyr::mutate(
          keep = dplyr::case_when(
            .data$flag == 1 & .data$row == min(.data$row, na.rm = TRUE) ~ TRUE,
            .data$flag == 0 ~ TRUE,
            .default = FALSE
          )
        ) |>
        dplyr::ungroup() |>
        dbplyr::window_order() |>
        dplyr::filter(.data$keep) |>
        dplyr::select(-"diff", -"flag", -"row", -"end_date", -"cohort_end_date_old", -"keep")
      
      counter <- counter + 1
    } else {
      tmpStudyPop <- tmpStudyPop |>
        dplyr::select(-"diff", -"flag", -"row", -"end_date")
      start <- FALSE
    }
  }
  return(tmpStudyPop)
}

applyEventEraCollapse <- function(studyPopulations, eraCollapseSize) {
  tblNames <- names(studyPopulations)
  studyPopulations  |>
    purrr::map(collapseEventEra, eraCollapseSize = eraCollapseSize)
}

getStudyPopulations <- function(andromeda) {
  tableNames <- names(andromeda)
  idx <- grep(pattern = "^study_population_.+$", tableNames)

  studyPops <- tableNames[idx] |>
    purrr::map(\(tbl) andromeda[[tbl]])

  names(studyPops) <- tableNames[idx]
  return(studyPops)
}

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "./dev/SQLite.sqlite")

cohorts <- CDMConnector::readCohortSet("./inst/exampleCohorts/") |>
  dplyr::select("cohort_definition_id", "cohort_name") |>
  dplyr::mutate(type = c(rep("event", 7), "target")) |>
  dplyr::mutate(min_era_duration = c(rep(7, 4), rep(14, 3), 30))

andromeda <- TreatmentPatterns::fetchCohortTable(
  connection = con,
  cdmSchema = DBI::Id("main"),
  writeSchema = DBI::Id("main"),
  cohortTables = DBI::Id(schema = "main", table = "cohort_table"),
  cohorts = cohorts
)

x <- andromeda |>
  createStudyPopulation() |>
  getStudyPopulations() |>
  applyMinEraDuration() |>
  applyEventEraCollapse(eraCollapseSize = 30)

df <- x[[1]] |>
  dplyr::group_by(.data$subject_id) |>
  dbplyr::window_order(.data$cohort_start_date, .data$cohort_end_date, .data$cohort_definition_id) |>
  dplyr::mutate(
    next_start = dplyr::lead(.data$cohort_start_date),
    next_end = dplyr::lead(.data$cohort_end_date),
    next_id = dplyr::lead(.data$cohort_definition_id),
    next_name = dplyr::lead(.data$cohort_name),
    days_to_next = as.integer(dplyr::lead(.data$cohort_start_date) - .data$cohort_end_date)
  ) |>
  dplyr::select("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date", "next_start", "next_end", "next_id", "next_name", "days_to_next") |>
  dplyr::mutate(
    type = dplyr::case_when(
      # + FRFS
      # |-----------------------------------|
      #         next_start       next_end
      #            |----------------|
      #            [----------------]
      .data$cohort_start_date <= .data$next_start
      & .data$cohort_end_date >= .data$next_end
      ~ "+",

      # * LRFS (FRLS)
      # |---------------------|
      #          next_start            next_end
      #             |---------------------|
      #             [---------]
      .data$cohort_start_date <= .data$next_start
      & .data$cohort_end_date <= .data$next_end
      & .data$cohort_end_date >= .data$next_start
      ~ "*"
    )
  ) |>
  dplyr::mutate(combine_order = dplyr::case_when(
    .data$type %in% c("+", "*") ~ dplyr::row_number()
  )) |>
  dplyr::mutate(to_combine = dplyr::case_when(
    .data$combine_order == min(.data$combine_order, na.rm = TRUE) ~ 1,
    .default = 0
  ))

frfs <- df |>
  dplyr::filter(.data$type == "+" & .data$to_combine == 1)

lrfs <- df |>
  dplyr::filter(.data$type == "*" & .data$to_combine == 1)

combinations <- frfs |>
  dplyr::mutate(
    cohort_start_date = .data$next_start,
    cohort_end_date = .data$next_end
  ) |>
  dplyr::union_all(
    lrfs |>
      dplyr::mutate(
        cohort_start_date = .data$next_start,
        cohort_end_date = .data$cohort_end_date
      )
  ) |>
  dplyr::mutate(
    cohort_definition_id = dplyr::sql("least(cohort_definition_id, next_id) || type || greatest(cohort_definition_id, next_id)")
  ) |>
  dplyr::ungroup() |>
  dbplyr::window_order() |>
  dplyr::select(-"")

df |> 
  dplyr::union_all(combinations) |>
  dplyr::group_by(.data$subject_id, .data$to_combine) |>
  dbplyr::window_order(.data$next_start, .data$next_end, .data$cohort_definition_id) |>
  dplyr::filter(.data$subject_id == 3) |>
  dplyr::mutate(x = dplyr::lead(.data$type))
