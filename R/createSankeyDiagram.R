splitPathItems <- function(treatmentPathways) {
  data <- treatmentPathways %>%
    rowwise() %>%
    dplyr::mutate(path = stringr::str_split(.data$path, pattern = "-")) %>%
    dplyr::mutate(freq = as.integer(.data$freq))
  
  data <- data %>%
    tidyr::unnest_wider(path, names_sep = "")
  
  data <- data %>%
    dplyr::group_by_at(grep("path", names(data))) %>%
    dplyr::summarise(freq = sum(.data$freq), .groups = "drop")
  
  data[is.na(data)] <- "Stopped"
  return(data)
}

createLinks <- function(data) {
  result1 <- data %>%
    mutate(
      source = paste0("1.", .data$path1),
      target = paste0("2.", .data$path2)
    ) %>%
    select("source", "target", "freq")
  
  
  if (suppressWarnings(!is.null(data$path3))) {
    result2 <- data %>%
      mutate(
        source = paste0("2.", .data$path2),
        target = paste0("3.", .data$path3)
      ) %>%
      select("source", "target", "freq")
    
    links <- dplyr::bind_rows(
      result1, result2
    )
  } else {
    links <- result1
  }
  
  links <- links %>%
    dplyr::mutate(value = round(freq / sum(freq) * 100, 2)) %>%
    dplyr::select(-"freq")
}

doGroupCombinations <- function(treatmentPathways, groupCombinations) {
  if (groupCombinations) {
    treatmentPathways$path <- treatmentPathways$path %>%
      stringr::str_replace_all(
        pattern = ".+\\+.+",
        replacement = "Combination"
      )
  }
  return(treatmentPathways)
}

createLinkedData <- function(data) {
  links <- createLinks(data)

  nodes <- data.frame(
    names = c(links$source, links$target) %>% unique()
  )

  links$source <- lapply(links$source, nameToId, names = nodes$names) %>%
    unlist()

  links$target <- lapply(links$target, nameToId, names = nodes$names) %>%
    unlist()

  links <- links %>%
    dplyr::group_by(.data$source, .data$target) %>%
    dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
    as.data.frame()

  return(list(links = links, nodes = nodes))
}

nameToId <- function(item, names) {
  item <- item %>%
    stringr::str_replace(pattern = "\\(", replacement = "\\\\(") %>%
    stringr::str_replace(pattern = "\\)", replacement = "\\\\)") %>%
    stringr::str_replace(pattern = "\\+", replacement = "\\\\+") %>%
    stringr::str_replace(pattern = "\\&", replacement = "\\\\&") %>%
    stringr::str_replace(pattern = "\\.", replacement = "\\\\.")
  return(grep(sprintf("^%s$",item), names) - 1)
}

getColorPalette <- function(treatmentPathways) {
  palette <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  )
  
  n <- treatmentPathways$path |> 
    stringr::str_split(pattern = "-") |>
    unlist() |>
    unique() |>
    length()
  
  return(palette[0:n])
}

#' createSankeyDiagram
#' 
#' Create sankey diagram.
#'
#' @template param_treatmentPathways
#' @template param_groupCombinations
#' @param colors (`character(n)`) Vector of hex color codes.
#' @param ... Paramaters for \link[networkD3]{sankeyNetwork}.
#'
#' @return (`htmlwidget`)
#' @export
#'
#' @examples
#' # Dummy data, typically read from treatmentPathways.csv
#' treatmentPathways <- data.frame(
#'   path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
#'            "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
#'   freq = c(206, 6, 14, 48, 221),
#'   sex = rep("all", 5),
#'   age = rep("all", 5),
#'   index_year = rep("all", 5)
#' )
#' 
#' createSankeyDiagram(treatmentPathways)
createSankeyDiagram <- function(treatmentPathways, groupCombinations = FALSE, colors = NULL, ...) {
  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )
  
  if (is.null(colors)) {
    colors <- sprintf(
      'd3.scaleOrdinal([%s])',
      paste0('"', getColorPalette(treatmentPathways),'"', collapse = ", ")
    )
  }
  
  data <- splitPathItems(treatmentPathways)
  
  if (ncol(data) <= 2) {
    stop("Cannot compute Sankey Diagram as there is only one level in the data.")
  }
  
  linkedData <- createLinkedData(data)
  
  networkD3::sankeyNetwork(
    Links = linkedData$links,
    Nodes = linkedData$nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "names",
    units = "%",
    colourScale = colors,
    ...
  )
}

#' createSankeyDiagram2
#' 
#' DEPRECATED Create sankey diagram.
#'
#' @template param_treatmentPathways
#' @template param_groupCombinations
#' @param colors (`character(n)`) Vector of hex color codes.
#' @param ... Paramaters for \link[networkD3]{sankeyNetwork}.
#'
#' @return (`htmlwidget`)
#' @export
#'
#' @examples
#' # Dummy data, typically read from treatmentPathways.csv
#' treatmentPathways <- data.frame(
#'   path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
#'            "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
#'   freq = c(206, 6, 14, 48, 221),
#'   sex = rep("all", 5),
#'   age = rep("all", 5),
#'   index_year = rep("all", 5)
#' )
#' 
#' createSankeyDiagram(treatmentPathways)
createSankeyDiagram2 <- function(treatmentPathways, groupCombinations = FALSE, colors = NULL, ...) {
  warning("`createSankeyDiagram2()` is deprecated, please use `createSankeyDiagram()`")
  createSankeyDiagram(treatmentPathways, groupCombinations, colors, ...)
}
