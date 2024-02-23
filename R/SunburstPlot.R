#' @title SunburstPlot
#' 
#' @description
#' Class to handle the Sunburst plot of TreatmentPatterns.
#' 
#' @export
SunburstPlot <- R6::R6Class(
  classname = "SunburstPlot",
  inherit = InteracitvePlot,
  
  # Private ----
  private = list(
    ## Fields ----
    .jsShowLegend = "
    function(el, x) {
      d3.select(el).select('.sunburst-togglelegend').property('checked', true);
      d3.select(el).select('.sunburst-legend').style('visibility', '');
    }
    ",
    
    makePlot = function(input, name) {
      htmlwidgets::onRender(
        TreatmentPatterns::createSunburstPlot(
          treatmentPathways = private$.reactiveValues$filteredData$treatmentPathways %>%
            dplyr::filter(.data$db == name),
          groupCombinations = input[[private$.groupCombiOption]],
          colors = private$.reactiveValues$filteredData$labels,
          legend = list(w = 400),
          withD3 = TRUE
        ),
        jsCode = private$.jsShowLegend
      )
    }
  )
)