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
#' @title ShinyApp
#' 
#' @description
#' R6 ShinyApp class.
#' 
#' @field namespace Namespace of the shiny application.
#' @field inputHandler R6 InputHandler module.
#' @field sunburstPlot R6 sunburstPlot module.
#' @field sankeyDiagram R6 sankeyDiagram module.
#' @field characterizationPlots R6 CharacterizationPlots module.
#' 
#' @noRd
ShinyApp <- R6::R6Class(
  classname = "ShinyApp",
  
  # Public ----
  public = list(
    #' @description
    #' Initializer method.
    #'
    #' @param namespace (`character(1)`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(namespace) {
      private$.namespace <- namespace
      private$.inputHandler <- TreatmentPatterns::InputHandler$new(private$.namespace)
      private$.sunburstPlot <- TreatmentPatterns::SunburstPlot$new(private$.namespace)
      private$.sankeyDiagram <- TreatmentPatterns::SankeyDiagram$new(private$.namespace)
      private$.characterizationPlots <- TreatmentPatterns::CharacterizationPlots$new(private$.namespace)
      return(invisible(self))
    },
    
    #' @description
    #' UI method.
    #' 
    #' @return (`shinyDashboard`)
    ui = function() {
      shinydashboard::dashboardPage(
        self$uiHeader(),
        self$uiMenu(),
        self$uiBody()
      )
    },
    
    #' @description
    #' Header method for the UI.
    #' 
    #' @return (`dashboardHeader`)
    uiHeader = function() {
      shinydashboard::dashboardHeader(title = "Results Explorer")
    },
    
    #' @description
    #' Menu method for the UI.
    #' 
    #' @return (`dashboardSidebar`)
    uiMenu = function() {
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          private$.inputHandler$uiMenu(),
          private$.sunburstPlot$uiMenu(tabName = "sunburst"),
          private$.sankeyDiagram$uiMenu(tabName = "sankey"),
          private$.characterizationPlots$uiMenu()
        ),
        private$.inputHandler$uiDatabaseSelector()
      )
    },
    
    #' @description
    #' Body method for the UI.
    #' 
    #' @return (`dashboardBody`)
    uiBody = function() {
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          private$.inputHandler$uiBody(),
          private$.sunburstPlot$uiBody(tabName = "sunburst"),
          private$.sankeyDiagram$uiBody(tabName = "sankey"),
          private$.characterizationPlots$uiBody()
        )
      )
    },
    
    #' @description
    #' Server method for the backend.
    #' 
    #' @return (`dashboardHeader`)
    server = function(input, output, session) {
      shiny::moduleServer(private$.namespace, function(input, output, session) {
        private$.inputHandler$setDataPath(tag = "uploadField", input = input, path = NULL)
        private$.inputHandler$server(input, output, session)
        private$.sunburstPlot$server(input, output, session, private$.inputHandler)
        private$.sankeyDiagram$server(input, output, session, private$.inputHandler)
        private$.characterizationPlots$server(input, output, session, private$.inputHandler)
      })
    }
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .namespace = "",
    .inputHandler = NULL,
    .sunburstPlot = NULL,
    .sankeyDiagram = NULL,
    .characterizationPlots = NULL,
    
    ## Methods ----
    finalize = function() {}
  ),
  
  # Active ----
  active = list(
    namespace = function() return(private$.namespace),
    inputHandler = function() return(private$.inputHandler),
    sunburstPlot = function() return(private$.sunburstPlot),
    sankeyDiagram = function() return(private$.sankeyDiagram),
    characterizationPlots = function() return(private$.characterizationPlots)
  )
)


#' launchResultsExplorer
#'
#' Launches the ResultExplorer shinyApp.
#'
#' @return (`shinyApp`)
#'
#' @export
#' 
#' @examples
#' if (interactive()) {
#'   launchResultsExplorer()
#' }
launchResultsExplorer <- function() {
  app <- ShinyApp$new("app")
  shiny::shinyApp(app$ui, app$server)
}
