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

#' plotEventDuration
#'
#' @param eventDurations (`data.frame`)
#' Contents of `summaryEventDuration.csv` file. 
#' @param minCellCount (`numeric(1)`:  `0`)
#' Min Cell Count per event group.
#' @param treatmentGroups (`character(1)`: `"both"`)
#' `"group"`: Only mono-, and combination-events.
#' `"individual"`: Only individual (combination) events.
#' `"both"`: Both mono-, and combination-events, and individual (combination) events.
#' @param eventLines (`numeric(n)`: `NULL`)
#' Event lines to include, i.e. `c(1, 2, 3)` includes first (`1`), second (`2`), and third (`3`) lines of events.
#' `NULL` will include all `eventLines`.
#' @param includeOverall (`logical(1)`: `TRUE`)
#' `TRUE`: Include an overall column with the `eventLines`.
#' `FALSE`: Exclude the overall column.
#'
#' @return `ggplot`
#' @export
#'
#' @examples
#' \donttest{
#' ableToRun <- all(
#'   require("CirceR", character.only = TRUE, quietly = TRUE),
#'   require("CDMConnector", character.only = TRUE, quietly = TRUE),
#'   require("TreatmentPatterns", character.only = TRUE, quietly = TRUE),
#'   require("dplyr", character.only = TRUE, quietly = TRUE)
#' )
#'
#' if (ableToRun) {
#'   withr::local_envvar(
#'     R_USER_CACHE_DIR = tempfile(),
#'     EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
#'   )
#'
#'   tryCatch({
#'     if (Sys.getenv("skip_eunomia_download_test") != "TRUE") {
#'       CDMConnector::downloadEunomiaData(overwrite = TRUE)
#'     }
#'   }, error = function(e) NA)
#'
#'   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#'   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main")
#'
#'   cohortSet <- readCohortSet(
#'     path = system.file(package = "TreatmentPatterns", "exampleCohorts")
#'   )
#'
#'   cdm <- generateCohortSet(
#'     cdm = cdm,
#'     cohortSet = cohortSet,
#'     name = "cohort_table"
#'   )
#'
#'   cohorts <- cohortSet %>%
#'     # Remove 'cohort' and 'json' columns
#'     select(-"cohort", -"json") %>%
#'     mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
#'     rename(
#'       cohortId = "cohort_definition_id",
#'       cohortName = "cohort_name",
#'     ) %>%
#'     select("cohortId", "cohortName", "type")
#'
#'   outputEnv <- computePathways(
#'     cohorts = cohorts,
#'     cohortTableName = "cohort_table",
#'     cdm = cdm
#'   )
#'
#'   results <- export(outputEnv)
#'
#'   plotEventDuration(
#'     eventDurations = results$summary_event_duration,
#'     minCellCount = 5,
#'     treatmentGroups = "group",
#'     eventLines = 1:4,
#'     includeOverall = FALSE
#'   )
#'
#'   Andromeda::close(outputEnv)
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
plotEventDuration <- function(eventDurations, minCellCount = 0, treatmentGroups = "both", eventLines = NULL, includeOverall = TRUE) {
  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertDataFrame(x = eventDurations, add = assertCol)
  checkmate::assertIntegerish(x = minCellCount, lower = 0, len = 1, null.ok = FALSE, add = assertCol)
  checkmate::assertChoice(x = treatmentGroups, choices = c("both", "group", "individual"), add = assertCol)
  checkmate::assertIntegerish(x = eventLines, min.len = 1, unique = TRUE, null.ok = TRUE, add = assertCol)
  checkmate::assertLogical(x = includeOverall, len = 1, null.ok = FALSE, add = assertCol)
  checkmate::reportAssertions(assertCol)

  #browser()
  eventDurations <- eventDurations %>%
    filter(
      .data$event_count >= minCellCount,
      case_when(
        treatmentGroups == "both" ~ .data$event_name == .data$event_name,
        treatmentGroups == "group" ~ .data$event_name %in% c("mono-event", "combination-event"),
        treatmentGroups == "individual" ~ !.data$event_name %in% c("mono-event", "combination-event")
      ),
      case_when(
        is.null(eventLines) ~ .data$event_name == .data$event_name,
        .default = .data$line %in% c(as.character(eventLines), "overall")
      ),
      case_when(
        includeOverall ~ .data$event_name == .data$event_name,
        .default = !.data$line == "overall"
      )
    )
  
  ggplot2::ggplot(data = eventDurations) +
    ggplot2::geom_boxplot(
      mapping = ggplot2::aes(
        y = event_name,
        xmin = duration_min,
        xlower = duration_q1,
        xmiddle = duration_median,
        xupper = duration_q2,
        xmax = duration_max
      ),
      stat = "identity"
    ) +
    ggplot2::facet_grid(.~line) +
    ggplot2::labs(
      title = "Duration of events per line",
      x = "time (days)"
    )
}
