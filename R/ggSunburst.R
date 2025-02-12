computeXCoords <- function(table, size) {
  table %>%
    dplyr::group_by(.data$sex, .data$age, .data$index_year) %>%
    dplyr::arrange(desc(.data$freq)) %>%
    dplyr::mutate(
      total = sum(.data$freq),
      frac = .data$freq / .data$total * size,
      xmax = cumsum(.data$frac),
      xmin = .data$xmax - .data$frac
    )
}

splitPaths <- function(table) {
  table %>%
    dplyr::mutate(path_id = row_number()) %>%
    tidyr::separate_longer_delim(cols = "pathway", delim = "-") %>%
    dplyr::group_by(.data$sex, .data$age, .data$index_year, .data$path_id) %>%
    dplyr::mutate(layer = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::rename(label = "pathway")
}

splitCombinations <- function(table, groupCombinations) {
  if (groupCombinations) {
    table %>%
      dplyr::mutate(is_combo = grepl(x = .data$label, pattern = "\\+")) %>%
      dplyr::group_by(.data$path_id, .data$layer, .data$sex, .data$age, .data$index_year) %>%
      dplyr::mutate(
        combo_id = dplyr::case_when(
          .data$is_combo ~ row_number(),
          .default = 1
        ),
        label = dplyr::case_when(
          .data$is_combo ~ "Combination",
          .default = .data$label
        )
      ) %>%
      dplyr::ungroup()
  } else {
    table %>%
      dplyr::mutate(is_combo = grepl(x = .data$label, pattern = "\\+")) %>%
      dplyr::group_by(.data$path_id, .data$layer, .data$sex, .data$age, .data$index_year) %>%
      tidyr::separate_longer_delim(cols = "label", delim = "+") %>%
      dplyr::mutate(combo_id = dplyr::case_when(
        .data$is_combo ~ row_number(),
        .default = 1
      )) %>%
      dplyr::ungroup()
  }
}

computeYCoords <- function(table, size) {
  table %>%
    dplyr::group_by(.data$path_id, .data$layer, .data$sex, .data$age, .data$index_year) %>%
    dplyr::mutate(max_combo = max(.data$combo_id)) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ymin = (1 / .data$max_combo) * size * (.data$combo_id - 1) + (size * .data$layer - size) + size,
      ymax = (1 / .data$max_combo) * size * (.data$combo_id) + (size * .data$layer - size) + size
    )
}

mergeLayerEvens <- function(table) {
  maxLayer <- max(table$layer)
  table %>%
    group_by(.data$label, .data$layer, .data$is_combo, .data$age, .data$sex, .data$index_year) %>%
    dplyr::mutate(
      freq = sum(.data$freq),
      next_xmin = dplyr::case_when(
        .data$layer != maxLayer ~ lead(.data$xmin, default = max(.data$xmax)),
        .default = lead(.data$xmin)
      ),
      xmin = dplyr::case_when(
        .data$next_xmin == .data$xmax ~ min(.data$xmin),
        .default = .data$xmin
      ),
      xmax = dplyr::case_when(
        .data$next_xmin == .data$xmax ~ max(.data$xmax),
        .default = .data$xmax
      )
    )
}

formatDataSunburst <- function(treatmentPathways, size, groupCombinations, mergeEvents) {
  res <- treatmentPathways %>%
    computeXCoords(size) %>%
    splitPaths() %>%
    splitCombinations(groupCombinations) %>%
    computeYCoords(size)
  
  res <- if (mergeEvents) {
    res %>%
      mergeLayerEvens()  
  } else {
    res
  }
  return(res)
}

makeGgSunburst <- function(data) {
  gg <- ggplot(data = data)
  
  nLayers <- max(data$layer)
  yMax <- max(data$ymax)
  
  for (layer in seq_len(nLayers)) {
    gg <- gg +
      geom_rect(
        data = data %>%
          dplyr::filter(.data$layer == layer),
        mapping = aes(
          ymin = ymin,
          ymax = ymax,
          # Width
          xmin = xmin,
          xmax = xmax,
          fill = label
        ),
        colour = "#000000"
      )
  }
  
  gg +
    coord_polar() +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) +
    ylim(0, yMax)
}

#' ggSunburst
#'
#' @template param_treatmentPathways
#' @template param_groupCombinations
#' @param unit (`character(1)`) Either `"count"` or `"percent"`, to scale the plot to.
#'
#' @returns (`gg`, `ggplot`)
#' @export
#'
#' @examples
#' # Dummy data, typically read from treatmentPathways.csv
#' treatmentPatwhays <- data.frame(
#'   pathway = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
#'            "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
#'   freq = c(206, 6, 14, 48, 221),
#'   sex = rep("all", 5),
#'   age = rep("all", 5),
#'   index_year = rep("all", 5)
#' )
#' 
#' ggSunburst(treatmentPatwhays)
ggSunburst <- function(treatmentPathways, groupCombinations = FALSE, unit = "percent", mergeEvents = FALSE) {
  size <- if (unit == "percent") {
    100
  } else if (unit == "count") {
    sum(treatmentPathways$freq)
  }

  formatDataSunburst(treatmentPathways, size, groupCombinations, mergeEvents) %>%
    makeGgSunburst()
}
