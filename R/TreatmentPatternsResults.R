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

#' @title TreatmentPatternsResults Class
#' 
#' @description
#' Houses the results of a `TreatmentPatterns` analysis. Each field corresponds
#' to a file. Plotting methods are provided.
#'
#' @export
TreatmentPatternsResults <- R6::R6Class(
  classname = "TreatmentPatternsResults",
  # Active Fields ----
  active = list(
    #' @field attrition (`data.frame`) 
    attrition = function() return(private$.attrition),

    #' @field metadata (`data.frame`) 
    metadata = function() return(private$.metadata),

    #' @field treatment_pathways (`data.frame`) 
    treatment_pathways = function() return(private$.treatmentPathways),

    #' @field summary_event_duration (`data.frame`) 
    summary_event_duration = function() return(private$.summaryEventDuration),

    #' @field counts_age (`data.frame`) 
    counts_age = function() return(private$.countsAge),

    #' @field counts_sex (`data.frame`) 
    counts_sex = function() return(private$.countsSex),

    #' @field counts_year (`data.frame`) 
    counts_year = function() return(private$.countsYear),

    #' @field cdm_source_info (`data.frame`)
    cdm_source_info = function() return(private$.cdmSourceInfo),

    #' @field analyses (`data.frame`)
    analyses = function() return(private$.analyses)
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @param attrition (`data.frame`) attrition result.
    #' @param metadata (`data.frame)`) metadata result.
    #' @param treatmentPathways (`data.frame)`) treatmentPathways result.
    #' @param summaryEventDuration (`data.frame)`) summaryEventDuration result.
    #' @param countsAge (`data.frame)`) countsAge result.
    #' @param countsSex (`data.frame)`) countsSex result.
    #' @param countsYear (`data.frame)`) countsYear result.
    #' @param cdmSourceInfo (`data.frame`) cdmSourceInfo result.
    #' @param analyses (`data.frame`) Analyses result.
    #' @param filePath (`character`) File path to either a directory or zip-file, containing the csv-files.
    initialize = function(
    attrition = NULL,
    metadata = NULL,
    treatmentPathways = NULL,
    summaryEventDuration = NULL,
    countsAge = NULL,
    countsSex = NULL,
    countsYear = NULL,
    cdmSourceInfo = NULL,
    analyses = NULL,
    filePath = NULL) {
      if (!is.null(filePath)) {
        self$load(filePath)
      } else {
        private$.attrition <- attrition
        private$.metadata <- metadata
        private$.treatmentPathways <- treatmentPathways
        private$.summaryEventDuration <- summaryEventDuration
        private$.countsAge <- countsAge
        private$.countsSex <- countsSex
        private$.countsYear <- countsYear
        private$.cdmSourceInfo <- cdmSourceInfo
        private$.analyses <- analyses
      }
    },

    #' @description
    #' Save the results as a zip-file.
    #'
    #' @param path (`character(1)`) Path to write to.
    #' @param name (`character(1)`) File name.
    #' @param verbose (`logical`: `TRUE`) Verbose messaging.
    #'
    #' @return `self`
    saveAsZip = function(path, name, verbose = TRUE) {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(path, len = 1, add = assertions)
      checkmate::assertCharacter(name, len = 1, add = assertions)
      checkmate::reportAssertions(assertions)

      dir.create(path, showWarnings = FALSE, recursive = TRUE)

      tempDir <- file.path(tempdir(), "tp-csv")
      dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
      outputPath <- file.path(path, name)
      self$saveAsCsv(path = tempDir, verbose = FALSE)
      invisible(zip(zipfile = outputPath, files = list.files(tempDir, full.names = TRUE), flags = "-j"))
      unlink(tempDir, recursive = TRUE)

      if (verbose) {
        message(sprintf("Wrote zip-file to: %s", path))
      }
      return(invisible(self))
    },
    
    #' @description
    #' Save the results as csv-files.
    #'
    #' @param path (`character(1)`) Path to write to.
    #' @param verbose (`logical`: `TRUE`) Verbose messaging.
    #'
    #' @return `self`
    saveAsCsv = function(path, verbose = TRUE) {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(path, len = 1, add = assertions)
      checkmate::reportAssertions(assertions)

      dir.create(path, showWarnings = FALSE, recursive = TRUE)

      write.csv(private$.attrition, file.path(path, "attrition.csv"), row.names = FALSE)
      write.csv(private$.metadata, file.path(path, "metadata.csv"), row.names = FALSE)
      write.csv(private$.treatmentPathways, file.path(path, "treatment_pathways.csv"), row.names = FALSE)
      write.csv(private$.summaryEventDuration, file.path(path, "summary_event_duration.csv"), row.names = FALSE)
      write.csv(private$.countsAge, file.path(path, "counts_age.csv"), row.names = FALSE)
      write.csv(private$.countsSex, file.path(path, "counts_sex.csv"), row.names = FALSE)
      write.csv(private$.countsYear, file.path(path, "counts_year.csv"), row.names = FALSE)
      write.csv(private$.cdmSourceInfo, file.path(path, "cdm_source_info.csv"), row.names = FALSE)
      write.csv(private$.analyses, file.path(path, "analyses.csv"), row.names = FALSE)
      
      if (verbose) {
        message(sprintf("Wrote csv-files to: %s", path))
      }
      return(invisible(self))
    },
    
    #' @description
    #' Upload results to a resultsDatabase using `ResultModelManager`.
    #'
    #' @param connectionDetails (`ConnectionDetails`) ConnectionDetails object from `DatabaseConnector`.
    #' @param schema (`character(1)`) Schema to write tables to.
    #' @param prefix (`character(1)`: `"tp_"`) Table prefix.
    #' @param overwrite (`logical(1)`: `TRUE`) Should tables be overwritten?
    #' @param purgeSiteDataBeforeUploading (`logical`: `FALSE`) Should site data be purged before uploading?
    #'
    #' @return `self`
    uploadResultsToDb = function(connectionDetails, schema, prefix = "tp_", overwrite = TRUE, purgeSiteDataBeforeUploading = FALSE) {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertClass(connectionDetails, classes = "ConnectionDetails", add = assertions)
      checkmate::assertCharacter(schema, len = 1, add = assertions)
      checkmate::assertCharacter(prefix, len = 1, add = assertions)
      checkmate::assertLogical(overwrite, len = 1, add = assertions)
      checkmate::reportAssertions(assertions)
      
      rmmInstalled <- require(
        "ResultModelManager",
        character.only = TRUE,
        quietly = TRUE,
        warn.conflicts = FALSE
      )
      
      if (rmmInstalled) {
        tempDir <- file.path(tempdir(), "tp-db")
        dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)
        self$saveAsCsv(path = tempDir, verbose = FALSE)
        ResultModelManager::uploadResults(
          connectionDetails = connectionDetails,
          specifications = getResultsDataModelSpecifications(),
          schema = schema,
          resultsFolder = tempDir,
          tablePrefix = prefix,
          purgeSiteDataBeforeUploading = purgeSiteDataBeforeUploading
        )
      } else {
        message("ResultModelManager is not installed. Install it with: remotes::install_github('OHDSI/ResultModelManager'")
      }
      unlink(tempDir, recursive = TRUE)
      return(invisible(self))
    },
    
    #' @description
    #' Load data from files.
    #'
    #' @param filePath (`character(1)`) Path to a directory or zip-file containing the result csv-files.
    #'
    #' @return `self`
    load = function(filePath) {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(filePath, len = 1, add = assertions)
      checkmate::reportAssertions(assertions)
      
      type <- private$assertSource(filePath)
      switch(
        type,
        zip = private$loadZip(filePath),
        csv = private$loadCsv(filePath)
      )
      return(invisible(self))
    },
    
    #' @description
    #' Wrapper for `TreatmentPatterns::createSunburstPlot()`, but with data filtering step.
    #'
    #' @param age (`character(1)`) Age group.
    #' @param sex (`character(1)`) Sex group.
    #' @param indexYear (`character(1)`) Index year group.
    #' @param nonePaths (`logical(1)`) Should `None` paths be included?
    #' @param ... Parameters for `TreatmentPatterns::createSunburstPlot()`
    #'
    #' @return `htmlwidget`
    plotSunburst = function(age = "all", sex = "all", indexYear = "all", nonePaths = FALSE, ...) {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(age, len = 1, add = assertions)
      checkmate::assertCharacter(sex, len = 1, add = assertions)
      checkmate::assertCharacter(indexYear, len = 1, add = assertions)
      checkmate::assertLogical(nonePaths, len = 1, add = assertions)
      checkmate::reportAssertions(assertions)
      
      none <- if (nonePaths) {
        ""
      } else {
        "None"
      }
      
      private$.treatmentPathways |>
        private$filterData(age, sex, indexYear, none) |>
        TreatmentPatterns::createSunburstPlot(...)
    },
    
    #' @description
    #' Wrapper for `TreatmentPatterns::createSankeyDiagram()`, but with data filtering step.
    #'
    #' @param age (`character(1)`) Age group.
    #' @param sex (`character(1)`) Sex group.
    #' @param indexYear (`character(1)`) Index year group.
    #' @param nonePaths (`logical(1)`) Should `None` paths be included?
    #' @param ... Parameters for `TreatmentPatterns::createSankeyDiagram()`
    #'
    #' @return `htmlwidget`
    plotSankey = function(age = "all", sex = "all", indexYear = "all", nonePaths = FALSE, ...) {
      assertions <- checkmate::makeAssertCollection()
      checkmate::assertCharacter(age, len = 1, add = assertions)
      checkmate::assertCharacter(sex, len = 1, add = assertions)
      checkmate::assertCharacter(indexYear, len = 1, add = assertions)
      checkmate::assertLogical(nonePaths, len = 1, add = assertions)
      checkmate::reportAssertions(assertions)
      
      none <- if (nonePaths) {
        ""
      } else {
        "None"
      }
      
      private$.treatmentPathways |>
        private$filterData(age, sex, indexYear, none) |>
        TreatmentPatterns::createSankeyDiagram(...)
    },
    
    #' @description
    #' Wrapper for `TreatmentPatterns::plotEventDuration()`.
    #'
    #' @param ... Parameters for `TreatmentPatterns::plotEventDuration()`
    #'
    #' @return `ggplot`
    plotEventDuration = function(...) {
      private$.summaryEventDuration |>
        TreatmentPatterns::plotEventDuration(...)
    }
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .attrition = NULL,
    .metadata = NULL,
    .treatmentPathways = NULL,
    .summaryEventDuration = NULL,
    .countsAge = NULL,
    .countsSex = NULL,
    .countsYear = NULL,
    .cdmSourceInfo = NULL,
    .analyses = NULL,
    
    ## Methods ----
    assertSource = function(filePath) {
      if (endsWith(tolower(filePath), suffix = ".zip")) {
        return("zip")
      } else if (dir.exists(filePath)) {
        return("csv")
      } else {
        stop("Cannot assert type. A zip-file or a directory containing csv-files are supported")
      }
    },
    
    loadZip = function(filePath) {
      fileNames <- unzip(zipfile = filePath, list = TRUE)$Name
      files <- lapply(fileNames, function(file) {
        filePath |>
          unz(file) |>
          read.csv()
      })
      names(files) <- fileNames
      
      private$.attrition <- files$attrition.csv
      private$.metadata <- files$metadata.csv
      private$.treatmentPathways <- files$treatment_pathways.csv
      private$.summaryEventDuration <- files$summary_event_duration.csv
      private$.countsAge <- files$counts_age.csv
      private$.countsSex <- files$counts_sex.csv
      private$.countsYear <- files$counts_year.csv
      private$.cdmSourceInfo <- files$cdm_source_info.csv
      private$.analyses <- files$analyses.csv
    },
    
    loadCsv = function(filePath) {
      private$.attrition <- read.csv(file.path(filePath, "attrition.csv"))
      private$.metadata <- read.csv(file.path(filePath, "metadata.csv"))
      private$.treatmentPathways <- read.csv(file.path(filePath, "treatment_pathways.csv"))
      private$.summaryEventDuration <- read.csv(file.path(filePath, "summary_event_duration.csv"))
      private$.countsAge <- read.csv(file.path(filePath, "counts_age.csv"))
      private$.countsSex <- read.csv(file.path(filePath, "counts_sex.csv"))
      private$.countsYear <- read.csv(file.path(filePath, "counts_year.csv"))
      private$.cdmSourceInfo <- read.csv(file.path(filePath, "cdm_source_info.csv"))
      private$.analyses <- read.csv(file.path(filePath, "analyses.csv"))
    },
    
    filterData = function(data, age, sex, indexYear, none) {
      data %>%
        dplyr::filter(
          .data$age == age,
          .data$sex == sex,
          .data$index_year == indexYear,
          .data$pathway != none
        )
    }
  )
)
