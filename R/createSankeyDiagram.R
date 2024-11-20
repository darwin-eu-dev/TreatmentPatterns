# Copyright 2024 DARWIN EU®
#
# This file is part of TreatmentPatterns
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
  links <- lapply(seq_len(ncol(data) - 2), function(i) {
    df <- data[, c(i, i + 1, ncol(data))]
    names(df) <- c("source", "target", "freq")
    df <- df %>%
      dplyr::mutate(
        source = sprintf("%s.%s", i, .data$source),
        target = sprintf("%s.%s", i + 1, .data$target)
      )
    return(df)
  }) |>
    dplyr::bind_rows()
  
  links <- links %>%
    dplyr::mutate(value = round(freq / sum(freq) * 100, 2)) %>%
    dplyr::select(-"freq")
}

doGroupCombinations <- function(treatmentPathways, groupCombinations) {
  if (groupCombinations) {
    treatmentPathways$path <- treatmentPathways$path %>%
      stringr::str_replace_all(
        pattern = "((\\w+)?\\+\\w+)+",
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
    stringr::str_replace_all(pattern = "\\(", replacement = "\\\\(") %>%
    stringr::str_replace_all(pattern = "\\)", replacement = "\\\\)") %>%
    stringr::str_replace_all(pattern = "\\+", replacement = "\\\\+") %>%
    stringr::str_replace_all(pattern = "\\&", replacement = "\\\\&") %>%
    stringr::str_replace_all(pattern = "\\.", replacement = "\\\\.")
  return(grep(sprintf("^%s$",item), names) - 1)
}

validateCreateSankeyDiagram <- function() {
  args <- eval(
    expr = expression(mget(names(formals()))),
    envir = sys.frame(sys.nframe() - 1)
  )
  
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertNames(
    x = names(args$treatmentPathways),
    type = "named",
    must.include = c("path", "freq"),
    .var.name = "treatmentPathways"
  )
  
  checkmate::assertLogical(
    x = args$groupCombinations,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "groupCombinations"
  )
  
  # checkmate::assertCharacter(
  #   x = args$colors,
  #   null.ok = TRUE,
  #   add = assertCol,
  #   .var.name = "colors"
  # )
  
  checkmate::reportAssertions(assertCol)
}

setColourScale <- function(linkedData, colors) {
  domain <- stringr::str_split_i(linkedData$nodes$names, "\\d\\.", i = 2)

  labels <- if (!is.null(colors)) {
    labels <- if (is.list(colors)) {
      data.frame(
        name = c(names(colors), "Stopped"),
        col = c(unlist(colors), "#555555")
      )
    } else {
      labels <- data.frame(
        name = unique(domain),
        col = colors[c(0:length(unique(domain)))]
      )
    }
    
    if (length(labels$col) < length(unique(domain))) {
      warning(sprintf("Missing %s colour(s)", length(unique(domain)) - length(labels$col)))
    }
    
    linkedData$nodes$range <- labels$col[match(domain, labels$name)]
    
    return(
      sprintf(
        'd3.scaleOrdinal().domain([%s]).range([%s])',
        paste0("'", linkedData$nodes$names, "'", collapse = ", "),
        paste0("'", linkedData$nodes$range, "'", collapse = ", ")
      )
    )
  } else {
    return("d3.scaleOrdinal(d3.schemeCategory20)")
  }
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
  validateCreateSankeyDiagram()

  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )
  
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
    colourScale = setColourScale(linkedData, colors),
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
