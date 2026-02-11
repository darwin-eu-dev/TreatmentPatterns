# plotEventDuration

plotEventDuration

## Usage

``` r
plotEventDuration(
  eventDurations,
  minCellCount = 0,
  treatmentGroups = "both",
  eventLines = NULL,
  includeOverall = TRUE
)
```

## Arguments

- eventDurations:

  (`data.frame`) Contents of `summaryEventDuration.csv` file.

- minCellCount:

  (`numeric(1)`: `0`) Min Cell Count per event group.

- treatmentGroups:

  (`character(1)`: `"both"`) `"group"`: Only mono-, and
  combination-events. `"individual"`: Only individual (combination)
  events. `"both"`: Both mono-, and combination-events, and individual
  (combination) events.

- eventLines:

  (`numeric(n)`: `NULL`) Event lines to include, i.e. `c(1, 2, 3)`
  includes first (`1`), second (`2`), and third (`3`) lines of events.
  `NULL` will include all `eventLines`.

- includeOverall:

  (`logical(1)`: `TRUE`) `TRUE`: Include an overall column with the
  `eventLines`. `FALSE`: Exclude the overall column.

## Value

`ggplot`

## Examples

``` r
# \donttest{
ableToRun <- all(
  require("CirceR", character.only = TRUE, quietly = TRUE),
  require("CDMConnector", character.only = TRUE, quietly = TRUE),
  require("TreatmentPatterns", character.only = TRUE, quietly = TRUE),
  require("dplyr", character.only = TRUE, quietly = TRUE)
)

if (ableToRun) {
  withr::local_envvar(
    R_USER_CACHE_DIR = tempfile(),
    EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
  )

  tryCatch({
    if (Sys.getenv("skip_eunomia_download_test") != "TRUE") {
      CDMConnector::downloadEunomiaData(overwrite = TRUE)
    }
  }, error = function(e) NA)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main")

  cohortSet <- readCohortSet(
    path = system.file(package = "TreatmentPatterns", "exampleCohorts")
  )

  cdm <- generateCohortSet(
    cdm = cdm,
    cohortSet = cohortSet,
    name = "cohort_table"
  )

  cohorts <- cohortSet %>%
    # Remove 'cohort' and 'json' columns
    select(-"cohort", -"json") %>%
    mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
    rename(
      cohortId = "cohort_definition_id",
      cohortName = "cohort_name",
    ) %>%
    select("cohortId", "cohortName", "type")

  outputEnv <- computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm
  )

  results <- export(outputEnv)

  plotEventDuration(
    eventDurations = results$summary_event_duration,
    minCellCount = 5,
    treatmentGroups = "group",
    eventLines = 1:4,
    includeOverall = FALSE
  )

  Andromeda::close(outputEnv)
  DBI::dbDisconnect(con, shutdown = TRUE)
}
#> 
#> Download completed!
#> Creating CDM database /tmp/Rtmpczt2kT/file29be1a817b37/GiBleed_5.3.zip
#> ℹ Generating 8 cohorts
#> ℹ Generating cohort (1/8) - acetaminophen
#> ✔ Generating cohort (1/8) - acetaminophen [176ms]
#> 
#> ℹ Generating cohort (2/8) - amoxicillin
#> ✔ Generating cohort (2/8) - amoxicillin [157ms]
#> 
#> ℹ Generating cohort (3/8) - aspirin
#> ✔ Generating cohort (3/8) - aspirin [157ms]
#> 
#> ℹ Generating cohort (4/8) - clavulanate
#> ✔ Generating cohort (4/8) - clavulanate [147ms]
#> 
#> ℹ Generating cohort (5/8) - death
#> ✔ Generating cohort (5/8) - death [131ms]
#> 
#> ℹ Generating cohort (6/8) - doxylamine
#> ✔ Generating cohort (6/8) - doxylamine [166ms]
#> 
#> ℹ Generating cohort (7/8) - penicillinv
#> ✔ Generating cohort (7/8) - penicillinv [156ms]
#> 
#> ℹ Generating cohort (8/8) - viralsinusitis
#> ✔ Generating cohort (8/8) - viralsinusitis [230ms]
#> 
#> -- Qualifying records for cohort definitions: 1, 2, 3, 4, 5, 6, 7, 8
#>  Records: 14041
#>  Subjects: 2693
#> -- Removing records < minEraDuration (0)
#>  Records: 11386
#>  Subjects: 2159
#> >> Starting on target: 8 (viralsinusitis)
#> -- Removing events outside window (startDate: 0 | endDate: 0)
#>  Records: 8366
#>  Subjects: 2144
#> -- splitEventCohorts
#>  Records: 8366
#>  Subjects: 2144
#> -- No eras needed Collapsing, eraCollapse (30)
#>  Records: 8366
#>  Subjects: 2144
#> -- Iteration 1: minPostCombinationDuration (30), combinatinoWindow (30)
#>  Records: 558
#>  Subjects: 512
#> -- Iteration 2: minPostCombinationDuration (30), combinatinoWindow (30)
#>  Records: 554
#>  Subjects: 512
#> -- After Combination
#>  Records: 554
#>  Subjects: 512
#> -- filterTreatments (First)
#>  Records: 553
#>  Subjects: 512
#> -- Max path length (5)
#>  Records: 553
#>  Subjects: 512
#> -- treatment construction done
#>  Records: 553
#>  Subjects: 512
# }
```
