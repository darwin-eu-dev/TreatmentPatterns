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

#' @title InputHandler
#'
#' @include ShinyModule.R
#'
#' @description
#' Class to handle input from the user. Supports direct paths or input fields
#' through `setDataPath()`.\cr\cr
#' 
#' @field reactiveValues (`reactiveValues`)\cr
#' reactiveValues class created by \link[shiny]{reactiveValues}.
#' 
#' @export
InputHandler <- R6::R6Class(
  classname = "InputHandler",
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
    uiMenu = function(label = "File upload", tag = "fileUpload") {
      shinydashboard::menuItem(
        text = label,
        tabName = tag,
        icon = shiny::icon(lib = "glyphicon", name = "upload")
      )
    },

    #' @description
    #' Method to include a \link[shinydashboard]{tabItem} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function() {
      shinydashboard::tabItem(
        tabName = "fileUpload",
        private$uploadFile()
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
    #' @return (`NULL`)
    server = function(input, output, session) {
      private$dbSelector(input, output, session)
      private$fetchTreatmentPathways()
      private$fetchCountsAge()
      private$fetchCountsSex()
      private$fetchCountsYear()
      private$fetchMetadata()
      private$fetchSummaryStatsTherapyDuration()
    },
    
    #' @description
    #' Method to include a \link[shiny]{uiOutput} to select between multiple
    #' uploaded files.
    #'
    #' @return (`uiOutput`)
    uiDatabaseSelector = function() {
      shiny::uiOutput(outputId = shiny::NS(private$.namespace, "dbSelector"))
    },
    
    #' @description
    #' Method to dictate where the data is coming from, either from the `input`
    #' through the shiny application, or from a specified path. When one is
    #' provided, the other is ignored.
    #'
    #' @param tag (`character(1)`)\cr
    #' Tag to use internally in `input`.
    #' 
    #' @param input (`input`)\cr
    #' Input from the server function of the shiny app.
    #' 
    #' @param path (`character(1)`)\cr
    #' Path to a zip-file containing TreatmentPatterns output files.
    #'
    #' @return (`invisible(self)`)
    setDataPath = function(tag = "uploadField", input = NULL, path = NULL) {
      if (!is.null(input)) {
        shiny::observe({
          private$.reactiveValues$dataPath <- input[[tag]]$datapath
          private$.reactiveValues$dbNames <- input[[tag]]$name
        })
      } else if (!is.null(path)) {
        private$.reactiveValues$dataPath <- path
        private$.reactiveValues$dbNames <- basename(path)
      } else {
        stop("Cannot assert where data is comming from.")
      }
      return(invisible(self))
    }
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .reactiveValues = shiny::reactiveValues(
      dataPath = "",
      dbNames = NULL,
      treatmentPathways = NULL,
      countsAge = NULL,
      countsSex = NULL,
      countsYear = NULL,
      summaryStatsTherapyDuration = NULL,
      metadata = NULL
    ),
    
    ## Methods ----
    ### UI ----
    uploadFile = function() {
      shiny::tagList(
        shiny::fileInput(
          inputId = shiny::NS(private$.namespace, "uploadField"),
          label = "Upload TreatmentPatterns output zip-file(s).",
          accept = ".zip"
        )
      )
    },
    
    dbSelector = function(input, output, session) {
      shiny::observeEvent(self$reactiveValues$dataPath, {
        output$dbSelector <- renderUI({
          shiny::checkboxGroupInput(
            inputId = session$ns("dbSelector"),
            label = "Databases",
            choices = unlist(self$reactiveValues$dbNames),
            selected = unlist(self$reactiveValues$dbNames)
          )
        })
      })
    },
    
    ### Server ----
    fetchFile = function(fileName) {
      #browser()
      pairedList <- mapply(list, self$reactiveValues$dataPath, self$reactiveValues$dbNames)
      lapply(seq(1, length(pairedList), 2), function(i) {
        path <- pairedList[[i]]
        db <- pairedList[[i + 1]]
        if (endsWith(path, ".zip")) {
          return(private$fetchZip(fileName, path, db))
        } else if (dir.exists(path)) {
          return(private$fetchCSV(fileName, path, db))
        } else {
          return(NULL)
        }
      }) %>% dplyr::bind_rows()
    },
    
    fetchZip = function(fileName, path, db) {
      read.csv(unzip(
        zipfile = path,
        files = fileName,
        exdir = tempdir()
      )) %>%
      dplyr::mutate(db = db) %>%
      dplyr::bind_rows()
    },

    fetchCSV = function(fileName, path, db) {
      read.csv(file.path(path, fileName)) %>%
        dplyr::mutate(db = db) %>%
      dplyr::bind_rows()
    },
    
    fetchTreatmentPathways = function() {
      shiny::observeEvent(private$.reactiveValues$dataPath, {
        if (!is.null(private$.reactiveValues$dataPath)) {
          private$.reactiveValues$treatmentPathways <- private$fetchFile("treatmentPathways.csv")
        }
      })
    },
    
    fetchCountsAge = function() {
      shiny::observeEvent(private$.reactiveValues$dataPath, {
        if (!is.null(private$.reactiveValues$dataPath)) {
          private$.reactiveValues$countsAge <- private$fetchFile("countsAge.csv")
        }
      })
    },
    
    fetchCountsSex = function() {
      shiny::observeEvent(private$.reactiveValues$dataPath, {
        if (!is.null(private$.reactiveValues$dataPath)) {
          private$.reactiveValues$countsSex <- private$fetchFile("countsSex.csv")
        }
      })
    },
    
    fetchCountsYear = function() {
      shiny::observeEvent(private$.reactiveValues$dataPath, {
        if (!is.null(private$.reactiveValues$dataPath)) {
          private$.reactiveValues$countsYear <- private$fetchFile("countsYear.csv")
        }
      })
    },
    
    fetchMetadata = function() {
      shiny::observeEvent(private$.reactiveValues$dataPath, {
        if (!is.null(private$.reactiveValues$dataPath)) {
          private$.reactiveValues$metadata <- private$fetchFile("metadata.csv")
        }
      })
    },
    
    fetchSummaryStatsTherapyDuration = function() {
      shiny::observeEvent(private$.reactiveValues$dataPath, {
        if (!is.null(private$.reactiveValues$dataPath)) {
          private$.reactiveValues$summaryStatsTherapyDuration <- private$fetchFile("summaryStatsTherapyDuration.csv")
        }
      })
    }
  ),
  
  # Active ----
  active = list(
    reactiveValues = function() return(private$.reactiveValues)
  )
)
