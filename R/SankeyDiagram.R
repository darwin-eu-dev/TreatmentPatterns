#' @title SankeyDiagram
#' 
#' @description
#' Class to handle the Sankey diagram of TreatmentPatterns.
#' 
#' @export
SankeyDiagram <- R6::R6Class(
  classname = "SankeyDiagram",
  inherit = InteracitvePlot,
  
  private = list(
    makePlot = function(input, name) {
      TreatmentPatterns::createSankeyDiagram(
        treatmentPathways = private$.reactiveValues$filteredData$treatmentPathways %>%
          dplyr::filter(.data$db == name),
        groupCombinations = input[[private$.groupCombiOption]]
      )
    }
  )
)
