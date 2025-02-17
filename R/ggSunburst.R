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

formatDataSunburst <- function(treatmentPathways, size, groupCombinations) {
  treatmentPathways %>%
    computeXCoords(size) %>%
    splitPaths() %>%
    splitCombinations(groupCombinations) %>%
    computeYCoords(size)
}

makeGgSunburst <- function(data) {
  gg <- ggplot2::ggplot(data = data)
  
  nLayers <- max(data$layer)
  yMax <- max(data$ymax)
  
  for (layer in seq_len(nLayers)) {
    gg <- gg +
      ggplot2::geom_rect(
        data = data %>%
          dplyr::filter(.data$layer == layer),
        mapping = ggplot2::aes(
          ymin = .data$ymin,
          ymax = .data$ymax,
          # Width
          xmin = .data$xmin,
          xmax = .data$xmax,
          fill = .data$label
        ),
        colour = "#000000"
      )
  }
  
  gg +
    ggplot2::coord_polar() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::ylim(0, yMax)
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
ggSunburst <- function(treatmentPathways, groupCombinations = FALSE, unit = "percent") {
  size <- if (unit == "percent") {
    100
  } else if (unit == "count") {
    sum(treatmentPathways$freq)
  }

  formatDataSunburst(treatmentPathways, size, groupCombinations) %>%
    makeGgSunburst()
}
