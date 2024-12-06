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

#' @title CharacterizationPlots
#' 
#' @include ShinyModule.R
#' 
#' @description
#' Class to handle the characterization plots.
#' 
#' @export
CharacterizationPlots <- R6::R6Class(
  classname = "CharacterizationPlots",
  inherit = ShinyModule,
  
  # Public ----
  public = list(
    #' @description
    #' Method to include a \link[shinydashboard]{menuItem} to link to the body.
    #'
    #' @param label (`character(1)`)\cr
    #' Label to show for the `menuItem`.
    #' 
    #' @param tag (`character(1)`)\cr
    #' Tag to use internally in `input`.
    #' 
    #' @return (`menuItem`)
    uiMenu = function(label = "Characteristics", tag = "characteristics") {
      shinydashboard::menuItem(
        text = label,
        tabName = tag,
        icon = shiny::icon(lib = "glyphicon", name = "stats")
      )
    },
    
    #' @description
    #' Method to include a \link[shinydashboard]{tabItem} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function() {
      shinydashboard::tabItem(
        tabName = "characteristics",
        private$characteristics()
      )
    },
    
    #' @description
    #' Method to handle the back-end.
    #'
    #' @param input (`input`)\cr
    #' Input from the server function.
    #'
    #' @param output (`output`)\cr
    #' Output from the server function.
    #'
    #' @param session (`session`)\cr
    #' Session from the server function.
    #' 
    #' @param inputHandler (`inputHandler`)\cr
    #' \link[TreatmentPatterns]{InputHandler} class.
    #' 
    #' @return (`NULL`)
    server = function(input, output, session, inputHandler) {
      private$plotCharAge(output, inputHandler)
      private$plotCharSex(output, inputHandler)
      private$plotCharIndexYear(output, inputHandler)
    }
  ),
  
  # Private ----
  private = list(
    ## Methods ----
    characteristics = function() {
      shiny::tagList(
        shiny::fluidRow(
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              title = "Age",
              shiny::uiOutput(shiny::NS(private$.namespace, "charAgePlot"))
            ),
            shiny::tabPanel(
              title = "Sex",
              shiny::uiOutput(shiny::NS(private$.namespace, "charSexPlot"))
            ),
            shiny::tabPanel(
              title = "Index Year",
              shiny::uiOutput(shiny::NS(private$.namespace, "charIndexYearPlot"))
            )
          )
        )
      )
    },
    
    numerizeCounts = function(data) {
      data$n <- stringr::str_replace_all(data$n, pattern = "<", replacement = "") %>%
        as.numeric()
      return(data)
    },
    
    plotCharAge = function(output, inputHandler) {
      output$charAgePlot <- shiny::renderUI({
        p <- ggplot2::ggplot(private$numerizeCounts(inputHandler$reactiveValues$countsAge), mapping = ggplot2::aes(x = age, y = n)) +
          ggplot2::geom_bar(stat = "identity", fill = "#336B91") +
          ggplot2::facet_grid(db ~ .) +
          ggplot2::theme_bw()
        
        plotly::ggplotly(p)
      })
    },
    
    plotCharSex = function(output, inputHandler) {
      output$charSexPlot <- shiny::renderUI({
        p <- ggplot2::ggplot(private$numerizeCounts(inputHandler$reactiveValues$countsSex), mapping = ggplot2::aes(x = sex, y = n, fill = sex)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::facet_grid(db ~ .) +
          ggplot2::scale_fill_manual(values = c("#336B91", "#EB6622")) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "none")
        
        plotly::ggplotly(p)
      })
    },
    
    plotCharIndexYear = function(output, inputHandler) {
      output$charIndexYearPlot <- shiny::renderUI({
        p <- ggplot2::ggplot(private$numerizeCounts(inputHandler$reactiveValues$countsYear), mapping = ggplot2::aes(x = index_year, y = n)) +
          ggplot2::geom_bar(stat = "identity", fill = "#336B91") +
          ggplot2::facet_grid(db ~ .) +
          ggplot2::theme_bw()
        
        plotly::ggplotly(p)
      })
    }
  )
)
