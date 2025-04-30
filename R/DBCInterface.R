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

#' @title DBCInterface
#'
#' @include CDMInterface.R
#'
#' @description
#' Decorated `CDMInterface` class for `DatabaseConnector`
#'
#' @noRd
DBCInterface <- R6::R6Class(
  classname = "DBCInterface",
  inherit = CDMInterface,

  # Active ----
  active = list(
    #' @field connectionDetails `ConnectionDetails`
    connectionDetails = function() return(private$.connectionDetails),

    #' @field cdmSchema `character(1)`
    cdmSchema = function() return(private$.cdmSchema),

    #' @field resultSchema `character(1)`
    resultSchema = function() return(private$.resultSchema),

    #' @field tempEmulationSchema `character(1)`
    tempEmulationSchema = function() return(private$.tempEmulationSchema)
  ),

  # Public ----
  public = list(
    ## Methods ----
    #' @description
    #' Initializer method
    #'
    #' @template param_connectionDetails
    #' @template param_cdmSchema
    #' @template param_resultSchema
    #' @param tempEmulationSchema Schema used to emulate temp tables.
    #'
    #' @return (`invisible(self)`)
    initialize = function(connectionDetails, cdmSchema, resultSchema, tempEmulationSchema = NULL) {
      private$.connectionDetails <- connectionDetails
      if (!is.null(private$.connectionDetails)) {
        private$.connection <- DatabaseConnector::connect(private$.connectionDetails)
      }
      private$.cdmSchema <- cdmSchema
      private$.resultSchema <- resultSchema
      private$.tempEmulationSchema <- tempEmulationSchema
    },

    
    #' @description
    #' Fetch specified cohort IDs from a specified cohort table
    #'
    #' @template param_cohorts
    #' @template param_cohortTableName
    #' @template param_andromeda
    #' @param andromedaTableName (`character(1)`)\cr
    #' Name of the table in the Andromeda object where the data will be loaded.
    #' @template param_minEraDuration
    #'
    #' @return (`andromeda`)
    fetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      targetCohortId <- getCohortIds(cohorts, "target")
      
      n <- lapply(cohortTableName, function(cohortTable) {
        DatabaseConnector::renderTranslateQuerySql(
          connection = private$.connection,
          sql = "
        SELECT COUNT(*)
        FROM @resultSchema.@cohortTable
        WHERE cohort_definition_id IN (@cohortIds)
        GROUP BY subject_id;",
          resultSchema = private$.resultSchema,
          cohortTable = cohortTable,
          cohortIds = cohorts$cohortId
        ) |>
          unlist() |>
          as.numeric()
      }) |>
        unlist()
      
      private$dbAppendAttrition(n, andromeda, sort(cohorts$cohortId))
      
      # Select relevant data
      sql <- lapply(cohortTableName, function(tableName) {
        SqlRender::loadRenderTranslateSql(
          sqlFilename = "selectData.sql",
          packageName = "TreatmentPatterns",
          dbms = private$.connection@dbms,
          tempEmulationSchema = private$.tempEmulationSchema,
          resultSchema = private$.resultSchema,
          cdmSchema = private$.cdmSchema,
          cohortTable = tableName,
          cohortIds = cohorts$cohortId,
          minEraDuration = minEraDuration
        )
      })
      
      renderedSql <- paste(sql, collapse = "\nUNION ALL\n")
      
      DatabaseConnector::renderTranslateExecuteSql(
        connection = private$.connection,
        oracleTempSchema = private$.tempEmulationSchema,
        sql = sprintf(
          "DROP TABLE IF EXISTS #tp_dbc_cohort_table;
          
          SELECT *
          INTO #tp_dbc_cohort_table
          FROM (
            %s
          ) a;",
          renderedSql
        ),
        tempEmulationSchema = private$.tempEmulationSchema
      )
      
      DatabaseConnector::renderTranslateQuerySqlToAndromeda(
        connection = private$.connection,
        andromeda = andromeda,
        andromedaTableName = andromedaTableName,
        tempEmulationSchema = private$.tempEmulationSchema,
        sql = "
        SELECT 
          #tp_dbc_cohort_table.cohort_definition_id AS cohort_definition_id,
          #tp_dbc_cohort_table.subject_id AS subject_id,
          #tp_dbc_cohort_table.cohort_start_date,
          #tp_dbc_cohort_table.cohort_end_date,
          #tp_dbc_cohort_table.age,
          #tp_dbc_cohort_table.sex,
          #tp_dbc_cohort_table.subject_id_origin
        FROM #tp_dbc_cohort_table
        INNER JOIN (
          SELECT #tp_dbc_cohort_table.subject_id
          FROM #tp_dbc_cohort_table
          WHERE #tp_dbc_cohort_table.cohort_definition_id IN (@targetCohortId)
        ) AS cross_sec
        ON cross_sec.subject_id = #tp_dbc_cohort_table.subject_id",
        targetCohortId = targetCohortId
      )
      
      names(andromeda[[andromedaTableName]]) <- tolower(names(andromeda[[andromedaTableName]]))
      
      if (utils::packageVersion("Andromeda") >= package_version("1.0.0")) {
        andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
          dplyr::mutate(
            cohort_start_date = dplyr::sql("datediff('day', DATE '1970-01-01', cohort_start_date)"),
            cohort_end_date = dplyr::sql("datediff('day', DATE '1970-01-01', cohort_end_date)")
          )
      } else {
        andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
          dplyr::mutate(
            cohort_start_date = as.integer(.data$cohort_start_date),
            cohort_end_date = as.integer(.data$cohort_end_date)
          )
      }
      
      n <- andromeda[[andromedaTableName]] %>%
        dplyr::group_by(.data$subject_id) %>% 
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull()
      
      appendAttrition(
        toAdd = data.frame(
          number_records = sum(n),
          number_subjects = length(n),
          reason_id = 2,
          reason = sprintf("Removing records < minEraDuration (%s)", minEraDuration),
          time_stamp = as.numeric(Sys.time())
        ),
        andromeda = andromeda
      )
      return(andromeda)
    },

    #' @description
    #' Fetch cdm_source from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`andromeda`)
    fetchCdmSource = function(andromeda) {
      renderedSql <- SqlRender::render(
        sql = "SELECT * FROM @cdmSchema.cdm_source;",
        cdmSchema = private$.cdmSchema
      )
      
      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = private$.connection@dbms
      )
      
      DatabaseConnector::querySqlToAndromeda(
        connection = private$.connection,
        sql = translatedSql,
        andromeda = andromeda,
        andromedaTableName = "cdm_source_info"
      )
      names(andromeda$cdm_source_info) <- tolower(names(andromeda$cdm_source_info))
      return(andromeda)
    },

    #' @description
    #' Destroys instance
    #' 
    #' @return (NULL)
    disconnect = function() {
      if (!is.null(private$.connection)) {
        DatabaseConnector::disconnect(private$.connection)
      }
    }
  ),

  # Private ----
  private = list(
    ## Fields ----
    .connectionDetails = NULL,
    .connection = NULL,
    .cdmSchema = NULL,
    .resultSchema = NULL,
    .tempEmulationSchema = NULL
  )
)
