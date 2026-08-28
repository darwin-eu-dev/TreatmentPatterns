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
    #' @field analyses (`data.frame`)
    analyses = function(analyses) {
      if (missing(analyses)) {
        return(private$.analyses)
      } else {
        checkmate::checkDataFrame(
          x = analyses,
          col.names = c(
            "analysis_id", "description", "result_id"
          )
        )
        private$.analyses <- analyses
      }
    },
    
    #' @field treatment_pathways (`data.frame`)
    treatment_pathways = function(treatment_pathways) {
      if (missing(treatment_pathways)) {
        return(private$.treatment_pathways)
      } else {
        checkmate::checkDataFrame(
          x = treatment_pathways,
          col.names = c(
            "pathway", "freq", "age", "sex", "index_year", "analysis_id",
            "target_cohort_id", "target_cohort_name", "result_id"
          )
        )
        private$.treatment_pathways <- treatment_pathways
      }
    },
    
    #' @field summary_event_duration (`data.frame`)
    summary_event_duration = function(summary_event_duration) {
      if (missing(summary_event_duration)) {
        return(private$.summary_event_duration)
      } else {
        checkmate::checkDataFrame(
          x = summary_event_duration,
          col.names = c(
            "event_name", "duration_min", "duration_q1", "duration_median",
            "duration_q2", "duration_max", "duration_average", "duration_sd",
            "event_count", "line", "analysis_id", "target_cohort_id",
            "target_cohort_name", "result_id"
          )
        )
        private$.summary_event_duration <- summary_event_duration
      }
    },
    
    #' @field counts_age (`data.frame`)
    counts_age = function(counts_age) {
      if (missing(counts_age)) {
        return(private$.counts_age)
      } else {
        checkmate::assertDataFrame(
          x = counts_age,
          col.names = c(
            "age", "n", "analysis_id", "target_cohort_id",
            "target_cohort_name", "result_id"
          )
        )
        private$.counts_age <- counts_age
      }
    },
    
    #' @field counts_sex (`data.frame`)
    counts_sex = function(counts_sex) {
      if (missing(counts_sex)) {
        return(private$.counts_sex)
      } else {
        checkmate::assertDataFrame(
          x = counts_sex,
          col.names = c(
            "sex", "n", "analysis_id", "target_cohort_id",
            "target_cohort_name", "result_id"
          )
        )
        private$.counts_sex <- counts_sex
      }
    },
    
    #' @field counts_year (`data.frame`)
    counts_year = function(counts_year) {
      if (missing(counts_year)) {
        return(private$.counts_year)
      } else {
        checkmate::checkDataFrame(
          x = counts_year,
          col.names = c(
            "index_year", "n", "analysis_id", "target_cohort_id",
            "target_cohort_name", "result_id"
          )
        )
        private$.counts_year <- counts_year
      }
    },
    
    #' @field attrition (`data.frame`)
    attrition = function(attrition) {
      if (missing(attrition)) {
        return(private$.attrition)
      } else {
        checkmate::checkDataFrame(
          x = attrition,
          col.names = c(
            "number_records", "number_subjects", "reason_id", "reason",
            "time_stamp", "analysis_id", "target_cohort_id",
            "target_cohort_name", "result_id"
          )
        )
        private$.attrition <- attrition
      }
    },
    
    #' @field metadata (`data.frame`)
    metadata = function(metadata) {
      if (missing(metadata)) {
        return(private$.metadata)
      } else {
        checkmate::checkDataFrame(
          x = metadata,
          col.names = c(
            "execution_start", "package_version", "r_version", "platform",
            "execution_end", "analysis_id", "result_id"
          )
        )
        private$.metadata <- metadata
      }
    },
    
    #' @field arguments (`data.frame`)
    arguments = function(arguments) {
      if (missing(arguments)) {
        return(private$.arguments)
      } else {
        checkmate::checkDataFrame(
          x = arguments,
          col.names = c(
            "analysis_id", "arguments", "result_id"
          )
        )
        private$.arguments <- arguments
      }
    },
    
    #' @field cdm_source_info (`data.frame`)
    cdm_source_info = function(cdm_source_info) {
      if (missing(cdm_source_info)) {
        return(private$.cdm_source_info)
      } else {
        checkmate::assertDataFrame(
          x = cdm_source_info,
          col.names = c(
            "cdm_source_name", "cdm_source_abbreviation", "cdm_holder",
            "source_description", "source_documentation_reference",
            "cdm_etl_reference", "source_release_date", "cdm_release_date",
            "cdm_version", "cdm_version_concept_id", "vocabulary_version",
            "analysis_id", "result_id"
          )
        )
        private$.cdm_source_info <- cdm_source_info
      }
    }
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
    #' @param arguments (`list`) Named list of arguments used.
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
    arguments = NULL,
    filePath = NULL) {
      if (!is.null(filePath)) {
        self$load(filePath)
      } else {
        private$.attrition <- attrition
        private$.metadata <- metadata
        private$.treatment_pathways <- treatmentPathways
        private$.summary_event_duration <- summaryEventDuration
        private$.counts_age <- countsAge
        private$.counts_sex <- countsSex
        private$.counts_year <- countsYear
        private$.cdm_source_info <- cdmSourceInfo
        private$.analyses <- analyses
        private$.arguments = arguments
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
        message(sprintf("Wrote zip-file to: %s", normalizePath(path)))
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
      write.csv(private$.treatment_pathways, file.path(path, "treatment_pathways.csv"), row.names = FALSE)
      write.csv(private$.summary_event_duration, file.path(path, "summary_event_duration.csv"), row.names = FALSE)
      write.csv(private$.counts_age, file.path(path, "counts_age.csv"), row.names = FALSE)
      write.csv(private$.counts_sex, file.path(path, "counts_sex.csv"), row.names = FALSE)
      write.csv(private$.counts_year, file.path(path, "counts_year.csv"), row.names = FALSE)
      write.csv(private$.cdm_source_info, file.path(path, "cdm_source_info.csv"), row.names = FALSE)
      write.csv(private$.analyses, file.path(path, "analyses.csv"), row.names = FALSE)
      write.csv(private$.arguments, file.path(path, "arguments.csv"), row.names = FALSE)

      if (verbose) {
        message(sprintf("Wrote csv-files to: %s", normalizePath(path)))
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
        unlink(tempDir, recursive = TRUE)
      } else {
        message("ResultModelManager is not installed. Install it with: remotes::install_github('OHDSI/ResultModelManager'")
      }
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

      private$.treatment_pathways |>
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
      
      private$.treatment_pathways |>
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
      private$.summary_event_duration |>
        TreatmentPatterns::plotEventDuration(...)
    },

    #' @description
    #' Transforms the results to a `SummarisedResult` object.
    #'
    #' @returns `SummarisedResult`
    transformToSummarisedResult = function() {
      if (require("omopgenerics", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
        omopgenerics::bind(
          private$summariseTreatmentPathways(),
          private$summariseSummaryEventDuration(),
          private$summariseCounts(),
          private$summariseAttrition(),
          private$summariseArguments(),
          private$summariseAnalyses(),
          private$summariseMetadata()
        )
      } else {
        stop("`omopgenerics` is not installed. You can install with with: install.packages('omopgenerics')")
      }
    },

    importSummarisedResult = function(summarisedResult) {
      if (require("omopgenerics", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
        # summarisedResult |>
        #   tidyr::pivot_wider(
        #     names_from = "strata_name",
        #     values_from = "strata_level"
        #   ) |>
        #   tidyr::pivot_wider(
        #     names_from = "group_name",
        #     values_from = "group_level"
        #   ) |>
        #   tidyr::pivot_wider(
        #     names_from = "estimate_name",
        #     values_from = "estimate_value"
        #   ) |>
        #   tidyr::pivot_wider(
        #     names_from = "additional_name",
        #     values_from = "additional_level"
        #   ) |>
        #   dplyr::select(
        #     pathway = "variable_level",
        #     "freq",
        #     "age",
        #     "sex",
        #     "index_year",
        #     analysis_id = "result_id",
        #     "target_cohort_id",
        #     "target_cohort_name"
        #   ) |>
        #   dplyr::mutate(
        #     freq = as.integer(.data$freq),
        #     target_cohort_id = as.integer(.data$target_cohort_id)
        #   )
      } else {
        stop("`omopgenerics` is not installed. You can install with with: install.packages('omopgenerics')")
      }
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .attrition = NULL,
    .metadata = NULL,
    .treatment_pathways = NULL,
    .summary_event_duration = NULL,
    .counts_age = NULL,
    .counts_sex = NULL,
    .counts_year = NULL,
    .cdm_source_info = NULL,
    .analyses = NULL,
    .arguments = NULL,

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
      private$.treatment_pathways <- files$treatment_pathways.csv
      private$.summary_event_duration <- files$summary_event_duration.csv
      private$.counts_age <- files$counts_age.csv
      private$.counts_sex <- files$counts_sex.csv
      private$.counts_year <- files$counts_year.csv
      private$.cdm_source_info <- files$cdm_source_info.csv
      private$.analyses <- files$analyses.csv
      private$.arguments <- files$arguments.csv
    },

    loadCsv = function(filePath) {
      private$.attrition <- read.csv(file.path(filePath, "attrition.csv"))
      private$.metadata <- read.csv(file.path(filePath, "metadata.csv"))
      private$.treatment_pathways <- read.csv(file.path(filePath, "treatment_pathways.csv"))
      private$.summary_event_duration <- read.csv(file.path(filePath, "summary_event_duration.csv"))
      private$.counts_age <- read.csv(file.path(filePath, "counts_age.csv"))
      private$.counts_sex <- read.csv(file.path(filePath, "counts_sex.csv"))
      private$.counts_year <- read.csv(file.path(filePath, "counts_year.csv"))
      private$.cdm_source_info <- read.csv(file.path(filePath, "cdm_source_info.csv"))
      private$.analyses <- read.csv(file.path(filePath, "analyses.csv"))
      private$.arguments <- read.csv(file.path(filePath, "arguments.csv"))
    },

    filterData = function(data, age, sex, indexYear, none) {
      data %>%
        dplyr::filter(
          .data$age == age,
          .data$sex == sex,
          .data$index_year == indexYear,
          .data$pathway != none
        )
    },

    summariseTreatmentPathways = function() {
      summarisedTreatmentPathways <- private$.treatmentPathways |>
        dplyr::inner_join(private$.analyses, by = "analysis_id") |>
        dplyr::inner_join(private$.cdmSourceInfo, by = "analysis_id") |>
        dplyr::mutate(
          freq = as.character(.data$freq),
          result_id = .data$analysis_id
        ) |>
        dplyr::rename(
          additional_level = "target_cohort_id",
          cdm_name = "cdm_source_name",
        ) |>
        tidyr::pivot_longer(
          cols = c("age", "sex", "index_year"),
          names_to = "strata_name",
          values_to = "strata_level"
        ) |>
        tidyr::pivot_longer(
          cols = c("freq"),
          names_to = "estimate_name",
          values_to = "estimate_value"
        ) |>
        tidyr::pivot_longer(
          cols = c("pathway"),
          names_to = "variable_name",
          values_to = "variable_level"
        ) |>
        dplyr::mutate(
          estimate_type = dplyr::case_when(
            .data$estimate_name == "pathway" ~ "character",
            .data$estimate_name == "freq" ~ "numeric"
          ),
          additional_name = "target_cohort_id"
        ) |>
        tidyr::pivot_longer(
          cols = "analysis_id",
          names_to = "group_name",
          values_to = "group_level"
        ) |>
        dplyr::select(
          "result_id", "cdm_name", "group_name", "group_level", "strata_name",
          "strata_level", "variable_name", "variable_level", "estimate_name",
          "estimate_type", "estimate_value", "additional_name", "additional_level"
        ) |>
        omopgenerics::newSummarisedResult()

      attr(summarisedTreatmentPathways, "settings") <- omopgenerics::settings(summarisedTreatmentPathways) |>
        dplyr::mutate(
          result_type = "treatment_pathways",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      return(summarisedTreatmentPathways)
    },

    summariseSummaryEventDuration = function() {
      summarisedSummaryEventDuration <- private$.summaryEventDuration |>
        dplyr::inner_join(private$.analyses, by = "analysis_id") |>
        dplyr::inner_join(private$.cdmSourceInfo, by = "analysis_id") |>
        dplyr::mutate(
          result_id = .data$analysis_id
        ) |>
        dplyr::rename(
          additional_level = "target_cohort_id",
          cdm_name = "cdm_source_name",
        ) |>
        tidyr::pivot_longer(
          cols = c("line"),
          names_to = "strata_name",
          values_to = "strata_level"
        ) |>
        tidyr::pivot_longer(
          cols = c("duration_min", "duration_q1", "duration_median", "duration_q2", "duration_max", "duration_average", "duration_sd", "event_count"),
          names_to = "estimate_name",
          values_to = "estimate_value"
        ) |>
        tidyr::pivot_longer(
          cols = c("event_name"),
          names_to = "variable_name",
          values_to = "variable_level"
        ) |>
        dplyr::mutate(
          estimate_type = "numeric",
          additional_name = "target_cohort_id"
        ) |>
        tidyr::pivot_longer(
          cols = "analysis_id",
          names_to = "group_name",
          values_to = "group_level"
        ) |>
        dplyr::select(
          "result_id", "cdm_name", "group_name", "group_level", "strata_name",
          "strata_level", "variable_name", "variable_level", "estimate_name",
          "estimate_type", "estimate_value", "additional_name", "additional_level"
        ) |>
        omopgenerics::newSummarisedResult()

      attr(summarisedSummaryEventDuration, "settings") <- omopgenerics::settings(summarisedSummaryEventDuration) |>
        dplyr::mutate(
          result_type = "summary_event_duration",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      return(summarisedSummaryEventDuration)
    },

    summariseCounts = function() {
      sumAge <- omopgenerics::transformToSummarisedResult(
        x = private$.countsAge,
        group = "analysis_id",
        strata = "age",
        additional = "target_cohort_id",
        estimates = "n"
      )

      attr(sumAge, "settings") <- omopgenerics::settings(sumAge) |>
        dplyr::mutate(
          result_type = "counts_age",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      sumSex <- omopgenerics::transformToSummarisedResult(
        x = private$.countsSex,
        group = "analysis_id",
        strata = "sex",
        additional = "target_cohort_id",
        estimates = "n"
      )

      attr(sumSex, "settings") <- omopgenerics::settings(sumSex) |>
        dplyr::mutate(
          result_type = "counts_sex",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      sumYear <- omopgenerics::transformToSummarisedResult(
        x = private$.countsYear,
        group = "analysis_id",
        strata = "index_year",
        additional = "target_cohort_id",
        estimates = "n"
      )

      attr(sumYear, "settings") <- omopgenerics::settings(sumYear) |>
        dplyr::mutate(
          result_type = "counts_year",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      return(omopgenerics::bind(sumAge, sumSex, sumYear))
    },

    summariseAttrition = function() {
      summarisedAttrition <- private$.attrition |>
        dplyr::inner_join(private$.analyses, by = "analysis_id") |>
        dplyr::inner_join(private$.cdmSourceInfo, by = "analysis_id") |>
        dplyr::mutate(
          result_id = .data$analysis_id
        ) |>
        dplyr::rename(
          additional_level = "target_cohort_id",
          cdm_name = "cdm_source_name",
        ) |>
        tidyr::pivot_longer(
          cols = "analysis_id",
          names_to = "group_name",
          values_to = "group_level"
        ) |>
        tidyr::pivot_longer(
          cols = c("number_records", "number_subjects", "time_stamp"),
          names_to = "estimate_name",
          values_to = "estimate_value"
        ) |>
        tidyr::pivot_longer(
          cols = c("reason"),
          names_to = "strata_name",
          values_to = "strata_level"
        ) |>
        dplyr::mutate(
          variable_name = "attrition",
          variable_level = NA,
          additional_name = "attrition",
          additional_value = NA,
          estimate_type = dplyr::case_when(
            .data$estimate_name == "number_records" ~ "integer",
            .data$estimate_name == "number_subjects" ~ "integer",
            .data$estimate_name == "time_stamp" ~ "numeric"
          )
        ) |>
        dplyr::select(
          "result_id", "cdm_name", "group_name", "group_level", "strata_name",
          "strata_level", "variable_name", "variable_level", "estimate_name",
          "estimate_type", "estimate_value", "additional_name", "additional_level"
        ) |>
        omopgenerics::newSummarisedResult()

      attr(summarisedAttrition, "settings") <- omopgenerics::settings(summarisedAttrition) |>
        dplyr::mutate(
          result_type = "attrition",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      return(summarisedAttrition)
    },

    summariseArguments = function() {
      summarisedArguments <- omopgenerics::transformToSummarisedResult(
        x = private$.arguments,
        group = "analysis_id",
        estimates = "arguments"
      )

      attr(summarisedArguments, "settings") <- omopgenerics::settings(summarisedArguments) |>
        dplyr::mutate(
          result_type = "arguments",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      return(summarisedArguments)
    },

    summariseAnalyses = function() {
      summarisedAnalyses <- omopgenerics::transformToSummarisedResult(
        x = private$.analyses,
        group = "analysis_id",
        estimates = "description"
      )

      attr(summarisedAnalyses, "settings") <- omopgenerics::settings(summarisedAnalyses) |>
        dplyr::mutate(
          result_type = "analyses",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      return(summarisedAnalyses)
    },

    summariseMetadata = function() {
      summarisedMetadata <- omopgenerics::transformToSummarisedResult(
        x = private$.metadata,
        group = "analysis_id",
        estimates = c("execution_start", "execution_end", "package_version", "r_version", "platform")
      )

      attr(x = summarisedMetadata, "settings") <- omopgenerics::settings(summarisedMetadata) |>
        dplyr::mutate(
          result_type = "analyses",
          package_name = "TreatmentPatterns",
          package_version = private$.metadata$package_version
        )

      return(summarisedMetadata)
    }
  )
)
