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

#' @title InteractivePlot
#' 
#' @include ShinyModule.R
#' 
#' @description
#' Class to handle the interactive plots of TreatmentPatterns (Sunburst plot &
#' Sankey diagram)
#' 
#' @noRd
InteracitvePlot <- R6::R6Class(
  classname = "InteracitvePlot",
  inherit = ShinyModule,

  # Public ----
  public = list(
    ## Fields ----
    #' @field colorPallete Colors to use in plot.
    colorPallete = c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
      "#1fff7f", "#ffff7f", "#2fcfaf", "#df6f2f", "#9f4f6f",
      "#8fcf5f", "#ef3f7f", "#7fff7f", "#bfcfbf", "#1f7fbf",
      "#7fbf4f", "#ff0fef", "#0f2fcf", "#7f2f8f", "#7fbfdf",
      "#6f4fbf", "#7fcf2f", "#ff7fff", "#df2f2f", "#efcfff"
    ),

    ## Methods ----
    initialize = function(app) {
      super$initialize(app)
      private$.sexOption <- sprintf("sexOption%s", class(self)[1])
      private$.ageOption <- sprintf("ageOption%s", class(self)[1])
      private$.indexYearOption <- sprintf("indexYearOption%s", class(self)[1])
      private$.noneOption <- sprintf("noneOption%s", class(self)[1])
      private$.groupCombiOption <- sprintf("groupCombiOption%s", class(self)[1])
    },

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
    uiMenu = function(text = "Plot", tabName = "plot") {
      shinydashboard::menuItem(
        text = text,
        tabName = tabName,
        icon = shiny::icon(lib = "glyphicon", name = "stats")
      )
    },

    #' @description
    #' Method to include a \link[shinydashboard]{tabItem} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function(tabName = "plot") {
      shinydashboard::tabItem(
        tabName = tabName,
        shiny::fluidRow(
          shinydashboard::box(
            width = "100%",
            shiny::column(width = 1, shiny::uiOutput(shiny::NS(private$.namespace, private$.sexOption))),
            shiny::column(width = 1, shiny::uiOutput(shiny::NS(private$.namespace, private$.ageOption))),
            shiny::column(width = 1, shiny::uiOutput(shiny::NS(private$.namespace, private$.indexYearOption))),
            shiny::column(width = 5, shiny::checkboxInput(shiny::NS(private$.namespace, private$.noneOption), label = "Exclude empty pathways")),
            shiny::column(width = 5, shiny::checkboxInput(shiny::NS(private$.namespace, private$.groupCombiOption), label = "Group combinations"))
          ),
          private$renderPlot()
        )
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
      private$setSexOptions(output, session, inputHandler)
      private$setAgeOptions(output, session, inputHandler)
      private$setIndexYearOptions(output, session, inputHandler)
      private$plot(input, output, inputHandler)
    }
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .sexOption = "",
    .ageOption = "",
    .indexYearOption = "",
    .noneOption = "",
    .groupCombiOption = "",
    .reactiveValues = shiny::reactiveValues(
      filteredData = NULL
    ),

    ## Interfaces ----
    renderPlot = function() {},
    plot = function(input, output, inputHandler) {},

    ## Methods ----
    checkInputOption = function(option) {
      if (is.null(option)) {
        "all"
      } else {
        option
      }
    },
    
    filterData = function(data, input) {
      none <- if (input[[private$.noneOption]]) {
        "None"
      } else {
        ""
      }
      
      data %>%
        dplyr::filter(.data$age == private$checkInputOption(input[[private$.ageOption]])) %>%
        dplyr::filter(.data$sex == private$checkInputOption(input[[private$.sexOption]])) %>%
        dplyr::filter(.data$index_year == private$checkInputOption(input[[private$.indexYearOption]])) %>%
        dplyr::filter(.data$pathway != none) %>%
        dplyr::filter(.data$db %in% input$dbSelector)
    },
    
    getLabelColors = function(data) {
      labels <- data %>%
        dplyr::pull(.data$pathway) %>%
        unique()

      range <- if (!is.null(labels)) {
        self$colorPallete[seq_len(length(labels))]
      }
      
      return(list(
        range = range,
        domain = labels
      ))
    },
    
    formatData = function(inputHandler, input) {
      treatmentPathways <- inputHandler$reactiveValues$treatmentPathways %>%
        private$filterData(input = input)
      labels <- private$getLabelColors(treatmentPathways)
      return(list(treatmentPathways = treatmentPathways, labels = labels))
    },
    
    setSexOptions = function(output, session, inputHandler) {
      shiny::observeEvent(inputHandler$reactiveValues$treatmentPathways, {
        output[[private$.sexOption]] <- renderUI({
          shiny::selectInput(
            inputId = session$ns(self$sexOption),
            label = "Sex",
            choices = unique(inputHandler$reactiveValues$treatmentPathways$sex),
            selected = "all"
          )
        })
      })
    },
    
    setAgeOptions = function(output, session, inputHandler) {
      shiny::observeEvent(inputHandler$reactiveValues$treatmentPathways, {
        output[[private$.ageOption]] <- renderUI({
          shiny::selectInput(
            inputId = session$ns(self$ageOption),
            label = "Age",
            choices = unique(inputHandler$reactiveValues$treatmentPathways$age),
            selected = "all"
          )
        })
      })
    },
    
    setIndexYearOptions = function(output, session, inputHandler) {
      shiny::observeEvent(inputHandler$reactiveValues$treatmentPathways, {
        output[[private$.indexYearOption]] <- renderUI({
          shiny::selectInput(
            inputId = session$ns(self$indexYearOption),
            label = "Index Year",
            choices = unique(inputHandler$reactiveValues$treatmentPathways$index_year),
            selected = "all"
          )
        })
      })
    }
  ),
  
  # Active ----
  active = list(
    sexOption = function() return(private$.sexOption),
    ageOption = function() return(private$.ageOption),
    indexYearOption = function() return(private$.indexYearOption),
    noneOption = function() return(private$.noneOption),
    reactiveValues = function() return(private$.reactiveValues)
  )
)
