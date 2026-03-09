mergeIndividualPathways <- function(treatmentPathways, strataX, strataY) {
  # layerOneTotal <- sum(treatmentPathways$freq)
  maxLayer <- max(sapply(strsplit(treatmentPathways$pathway, split = "-"), length))

  layerColumns <- sprintf("layer_%s", 1:maxLayer)
  
  naReplaceList <- as.list(rep("", maxLayer))
  names(naReplaceList) <- layerColumns
  
  dat <- treatmentPathways |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY))) |>
    dplyr::mutate(
      total = sum(.data$freq),
      path_to_sep = .data$pathway,
      path_id = dplyr::row_number()
    ) |>
    tidyr::separate_wider_delim(
      cols = "path_to_sep",
      delim = "-",
      names = layerColumns,
      too_few = "align_start"
    ) |>
    tidyr::replace_na(naReplaceList) |>
    dplyr::group_by(.data$layer_1) |>
    dplyr::mutate(l1_freq = sum(.data$freq)) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY))) |>
    dplyr::arrange(.data$l1_freq, !!!rlang::parse_exprs(layerColumns)) |>
    dplyr::mutate(
      frac = .data$freq / .data$total * 100,
      xmax = cumsum(.data$frac),
      xmin = .data$xmax - .data$frac
    ) |>
    tidyr::separate_longer_delim(cols = "pathway", delim = "-") |>
    dplyr::rename(event = "pathway") |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY)), .data$path_id) |>
    dplyr::mutate(layer = dplyr::row_number()) |>
    dplyr::ungroup()
  
  lapply(1:maxLayer, function(i) {
    layerDat <- dat |>
      dplyr::filter(.data$layer == i) |>
      dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY)), !!!rlang::parse_exprs(layerColumns[1:i])) |>
      dplyr::reframe(
        layer = .data$layer,
        event = .data$event,
        freq = sum(.data$freq),
        frac = sum(.data$frac),
        xmin = min(.data$xmin),
        xmax = max(.data$xmax)
      ) |>
      dplyr::distinct()
  }) |>
    dplyr::bind_rows()
}

splitCombinations <- function(treatmentPathways, strataX, strataY) {
  layerCols <- names(treatmentPathways)[grepl(pattern = "^layer_\\d$", names(treatmentPathways))]
  
  n <- sum(grepl(names(treatmentPathways), pattern = "^layer_\\d$"))
  
  treatmentPathways |>
    dplyr::mutate(
      event_to_split = .data$event
    ) |>
    tidyr::separate_longer_delim(cols = "event_to_split", delim = "+") |>
    dplyr::group_by(!!!rlang::parse_exprs(c(strataX, strataY)), !!!rlang::parse_exprs(layerCols)) |>
    dplyr::mutate(
      comb_id = dplyr::row_number(),
      comb_max = max(dplyr::row_number())
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ymin = 1 / .data$comb_max * (comb_id - 1) + .data$layer,
      ymax = 1 / .data$comb_max * comb_id + .data$layer
    ) |>
    dplyr::rename(
      event_org = "event",
      event = "event_to_split"
    )
}

plotSunburst <- function(treatmentPathways, strataX, strataY) {
  ggDat <- mergeIndividualPathways(treatmentPathways, strataX, strataY) |>
    splitCombinations(strataX, strataY)

  gg <- ggplot2::ggplot(data = ggDat)
  
  nLayers <- sum(grepl(pattern = "^layer_\\d$", names(ggDat)))
  
  for (i in 1:nLayers)
    gg <- gg + ggplot2::geom_rect(
      data = ggDat |>
        dplyr::filter(.data$layer == i),
      mapping = ggplot2::aes(
        ymin = .data$ymin,
        ymax = .data$ymax,
        xmin = .data$xmin,
        xmax = .data$xmax,
        fill = .data$event
      ),
      colour = "#000000"
    )

  gg +
    ggplot2::coord_polar() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::ylim(0, nLayers + 1) +
    ggplot2::facet_grid(
      rows = vars(!!!rlang::parse_exprs(strataY)),
      cols = vars(!!!rlang::parse_exprs(strataX))
    )
}

ggPlotSunburst2 <- function(treatmentPathways, minFreq = 0, strataX = "", strataY = "", style = "default") {
  gg <- treatmentPathways |>
    dplyr::filter(.data$freq >= minFreq) |>
    plotSunburst(strataX, strataY)

  if (minFreq >= 0) {
    nPaths <- treatmentPathways |>
      dplyr::filter(.data$freq < minFreq) |>
      nrow()
    message(sprintf("Filtered out %s pathways with a frequency < %s", nPaths, minFreq))
  }

  if (style == "darwin") {
    gg <- gg +
      visOmopResults::themeVisOmop(style = "darwin")
  }

  return(gg)
}

# tpr <- TreatmentPatterns::TreatmentPatternsResults$new(
#   filePath = system.file(
#     "DummyOutput", "output.zip",
#     package = "TreatmentPatterns"
#   )
# )
# 
# df <- tpr$treatment_pathways |>
#   dplyr::mutate(database = "foo") |>
#   dplyr::bind_rows(
#     tpr$treatment_pathways |>
#       dplyr::mutate(database = "bar")
#   )
# 
# ggPlotSunburst2(treatmentPathways = df, minFreq = 20, strataX = "database", style = "darwin")
