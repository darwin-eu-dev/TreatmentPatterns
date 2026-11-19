minCellCount <- 10

makePathways <- function(tbl, strata) {
  tbl |>
    dbplyr::window_order(.data$eventSeq) |>
    dplyr::summarise(
      pathway = dplyr::sql("string_agg(eventCohortName, '-')"),
      .by = dplyr::all_of("personId", "targetCohortId", "n_target", strata)
    ) |>
    dbplyr::window_order() |>
    dplyr::summarise(
      n = dplyr::n(),
      .by = dplyr::all_of("pathway", strata)
    )
}

prunePathways <- function(tbl, minCellCount, strata = NULL) {
  res <- tbl |>
    makePathways()

  nBelowMCC <- res |>
    dplyr::filter(.data$n < !!minCellCount) |>
    dplyr::count() |>
    dplyr::pull()

  while(nBelowMCC > 1) {
    res <- res |>
      dplyr::mutate(
        pathway_split = dplyr::sql("regexp_split_to_array(pathway, '-')")
      ) |>
      dplyr::mutate(
        n_updated = dplyr::case_when(
          length(.data$pathway_split) == 1 & .data$n < !!minCellCount ~ NA,
          .default = .data$n
        ),
        pruned_pathway_split = dplyr::case_when(
          length(.data$pathway_split) > 1 & .data$n < !!minCellCount ~ dplyr::sql("array_pop_back(pathway_split)"),
          .default = .data$pathway_split
        )
      ) |>
      dplyr::mutate(
        pathway = dplyr::sql("array_to_string(pruned_pathway_split, '-')")
      ) |>
      dplyr::summarise(
        n = sum(.data$n_updated, na.rm = TRUE),
        .by = dplyr::all_of("pathway", strata)
      )

    nBelowMCC <- res |>
      dplyr::filter(.data$n < !!minCellCount) |>
      dplyr::count() |>
      dplyr::pull()
  }

  res |>
    dplyr::collect()
}

res <- outputEnv$treatmentHistoryFinal |>
  prunePathways(minCellCount = 10)

