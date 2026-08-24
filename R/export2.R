collapsePaths <- function(treatmentHistory) {
  groups <- attr(treatmentHistory, "groups")[1, ]

  if (!is.null(groups)) {
    groups <- groups |>
      dplyr::select(-".rows") |>
      unlist() |>
      as.character()
  }

  treatmentHistory |>
    dplyr::group_by(.data$personId) |>
    dplyr::arrange(.data$eventSeq) |>
    dplyr::mutate(
      pathway = list(.data$eventCohortName[.data$eventSeq])
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$indexYear, .data$pathway) |>
    dplyr::mutate(freq = length(.data$personId)) |>
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    dplyr::mutate(pathway = paste(.data$pathway, collapse = "-")) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      n = dplyr::n(),
      .by = c("pathway", "targetCohortId", groups)
    )
}

export2 <- function(andromeda, strata = NULL) {
  strata <- list("age_group", "sex", "index_year", list("age_group", "sex", "index_year"))

  andromeda$treatmentHistoryFinal <- andromeda$treatmentHistoryFinal |>
    dplyr::mutate(
      age_group = dplyr::case_when(
        .data$age >= 0 & .data$age <= 17 ~ "0-17",
        .data$age >= 18 & .data$age <= 65 ~ "18-65",
        .data$age >= 66 ~ ">=66"
      ),

      index_year = as.character(as.integer(.data$indexYear))
    )

  strataCols <- strata |>
    unlist() |>
    unique()

  l <- rep("overall", length(strataCols)) |>
    as.list()

  names(l) <- strataCols

  overallDf <- andromeda$treatmentHistoryFinal |>
    dplyr::collect() |>
    collapsePaths()

  strata |>
    purrr::map(\(strataGroup) {
      andromeda$treatmentHistoryFinal |>
        dplyr::group_by(strataGroup) |>
        dplyr::collect()
    }) |>
    purrr::map(collapsePaths) |>
    purrr::reduce(bind_rows) |>
    dplyr::bind_rows(overallDf) |>
    tidyr::replace_na(replace = l)
}
