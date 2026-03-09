# library(ggplot2)
# library(dplyr)

# d <- mergeIndividualPathways(df)
# 
# d |>
#   group_by(.data$layer_1) |>
#   arrange(.data$freq)

mergeIndividualPathways <- function(treatmentPathways) {
  layerOneTotal <- sum(treatmentPathways$freq)
  maxLayer <- max(sapply(strsplit(treatmentPathways$pathway, split = "-"), length))
  
  layerColumns <- sprintf("layer_%s", 1:maxLayer)
  
  naReplaceList <- as.list(rep("", maxLayer))
  names(naReplaceList) <- layerColumns
  
  dat <- treatmentPathways |>
    dplyr::mutate(
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
    dplyr::arrange(.data$l1_freq, !!!rlang::parse_exprs(layerColumns)) |>
    dplyr::mutate(
      frac = .data$freq / layerOneTotal * 100,
      xmax = cumsum(.data$frac),
      xmin = .data$xmax - .data$frac
    ) |>
    tidyr::separate_longer_delim(cols = "pathway", delim = "-") |>
    dplyr::rename(event = "pathway") |>
    dplyr::group_by(.data$path_id) |>
    dplyr::mutate(layer = dplyr::row_number()) |>
    dplyr::ungroup()
  
  lapply(1:maxLayer, function(i) {
    layerDat <- dat |>
      dplyr::filter(.data$layer == i) |>
      dplyr::group_by(!!!rlang::parse_exprs(layerColumns[1:i])) |>
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

splitCombinations <- function(treatmentPathways) {
  layerCols <- names(treatmentPathways)[grepl(pattern = "^layer_\\d$", names(treatmentPathways))]
  
  n <- sum(grepl(names(treatmentPathways), pattern = "^layer_\\d$"))
  
  treatmentPathways |>
    dplyr::mutate(
      event_to_split = .data$event
    ) |>
    tidyr::separate_longer_delim(cols = "event_to_split", delim = "+") |>
    dplyr::group_by(!!!rlang::parse_exprs(layerCols)) |>
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

plotSunburst <- function(treatmentPathways) {
  ggDat <- mergeIndividualPathways(treatmentPathways) |>
    splitCombinations()
  
  gg <- ggplot(data = ggDat)
  
  nLayers <- sum(grepl(pattern = "^layer_\\d$", names(ggDat)))
  
  for (i in 1:nLayers)
    gg <- gg + geom_rect(
      data = ggDat |>
        dplyr::filter(.data$layer == i),
      mapping = aes(
        ymin = ymin,
        ymax = ymax,
        xmin = xmin,
        xmax = xmax,
        fill = event
      ),
      colour = "#000000"
    )
  
  gg +
    coord_polar() +
    theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ylim(0, nLayers + 1) +
    theme_void()
}

ggPlotSunburst2 <- function(treatmentPathways, minFreq = 0, strataX = NULL, strataY = NULL) {
  nAge <- length(unique(treatmentPathways$age))
  nSex <- length(unique(treatmentPathways$sex))
  nYear <- length(unique(treatmentPathways$index_year))
  
  colMulGroups <- c("age", "sex", "index_year")[as.logical((c(nAge, nSex, nYear) - 1))]
  colOneGroups <- c("age", "sex", "index_year")[!as.logical((c(nAge, nSex, nYear) - 1))]
  
  gg <- if (sum(nAge, nSex, nYear) == 3) {
    plotSunburst(treatmentPathways)
  } else if (length(colMulGroups) == 1) {
    filterDf <- lapply(colMulGroups, function(col) {
      treatmentPathways |>
        dplyr::summarise(
          !!rlang::sym(col) := min(.data[[col]])
        )
    }) |>
      dplyr::bind_cols()
    
    filterDf <- lapply(colOneGroups, function(col) {
      dplyr::tibble(!!rlang::sym(col) := "all")
    }) |>
      dplyr::bind_cols() |>
      dplyr::bind_cols(filterDf)
    
    warning(
      sprintf(
        "Multiple groups detected for columns: %s. Defaulting to: `%s`",
        paste(colMulGroups, collapse = ", "),
        paste(sprintf("%s == '%s'", colMulGroups, filterDf[colMulGroups]), collapse = ", ")
      )
    )
    
    treatmentPathways |>
      dplyr::right_join(filterDf) |>
      plotSunburst()
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
#   dplyr::filter(.data$freq >= 20) |>
#   dplyr::mutate(sex = "male") |>
#   dplyr::bind_rows(
#     tpr$treatment_pathways |>
#       dplyr::filter(.data$freq >= 20) |>
#       dplyr::mutate(sex = "female")
#   )
# 
# ggPlotSunburst2(treatmentPathways = df)
