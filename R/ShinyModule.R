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

#' @title ShinyModule
#'
#' @description
#' ShinyModule super class
#' 
#' @field namespace Namespace of the module.
#' 
#' @export
ShinyModule <- R6::R6Class(
  classname = "ShinyModule",
  
  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param namespace (`character(1)`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(namespace) {
      private$.namespace <- namespace
      self$validate()
      return(invisible(self))
    },

    #' @description
    #' Validator method
    #'
    #' @return (`invisible(self)`)
    validate = function() {
      private$assertDependencies()
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(private$.namespace, len = 1)
      checkmate::reportAssertions(assertions)
      return(invisible(self))
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
    uiMenu = function(label, tag) {},
    
    #' @description
    #' Method to include a \link[shinydashboard]{tabItem} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function() {},
    
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
    server = function(input, output, session) {}
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .namespace = "",
    
    assertDependencies = function() {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertTRUE(require("shiny", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("shinydashboard", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("ggplot2", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("plotly", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::assertTRUE(require("dplyr", quietly = TRUE, mask.ok = TRUE, character.only = TRUE))
      checkmate::reportAssertions(assertions)
    },

    ## Methods ----
    finalize = function() {}
  ),
  
  # Active ----
  active = list(
    namespace = function() return(private$.namespace)
  )
)
