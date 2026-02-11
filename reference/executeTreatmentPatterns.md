# executeTreatmentPatterns

Compute treatment patterns according to the specified parameters within
specified cohorts. For more customization, or investigation of patient
level outcomes, you can run
[computePathways](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/computePathways.md)
and
[export](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md)
separately.

## Usage

``` r
executeTreatmentPatterns(
  cohorts,
  cohortTableName,
  cdm = NULL,
  connectionDetails = NULL,
  cdmSchema = NULL,
  resultSchema = NULL,
  tempEmulationSchema = NULL,
  minEraDuration = 0,
  eraCollapseSize = 30,
  combinationWindow = 30,
  minCellCount = 5
)
```

## Arguments

- cohorts:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  Data frame containing the following columns and data types:

  cohortId `numeric(1)`

  :   Cohort ID's of the cohorts to be used in the cohort table.

  cohortName `character(1)`

  :   Cohort names of the cohorts to be used in the cohort table.

  type `character(1)` \["target", "event', "exit"\]

  :   Cohort type, describing if the cohort is a target, event, or exit
      cohort

- cohortTableName:

  (`character(1)`)  
  Cohort table name.

- cdm:

  (`CDMConnector::cdm_from_con()`: `NULL`)  
  Optional; Ignores `connectionDetails`, `cdmSchema`, and
  `resultSchema`.

- connectionDetails:

  ([`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html):
  `NULL`)  
  Optional; In congruence with `cdmSchema` and `resultSchema`. Ignores
  `cdm`.

- cdmSchema:

  (`character(1)`: `NULL`)  
  Optional; In congruence with `connectionDetails` and `resultSchema`.
  Ignores `cdm`.

- resultSchema:

  (`character(1)`: `NULL`)  
  Optional; In congruence with `connectionDetails` and `cdmSchema`.
  Ignores `cdm`.

- tempEmulationSchema:

  (`character(1)`) Schema to emulate temp tables.

- minEraDuration:

  (`integer(1)`: `0`)  
  Minimum time an event era should last to be included in analysis

- eraCollapseSize:

  (`integer(1)`: `30`)  
  Window of time between which two eras of the same event cohort are
  collapsed into one era

- combinationWindow:

  (`integer(1)`: `30`)  
  Window of time two event cohorts need to overlap to be considered a
  combination treatment

- minCellCount:

  (`integer(1)`: `5`)  
  Minimum count required per pathway. Censors data below `x` as `<x`.
  This minimum value will carry over to the sankey diagram and sunburst
  plot.

## Value

`TreatmentPatternsResults`

## Examples

``` r
# \donttest{
ableToRun <- all(
  require("CirceR", character.only = TRUE, quietly = TRUE),
  require("CDMConnector", character.only = TRUE, quietly = TRUE),
  require("TreatmentPatterns", character.only = TRUE, quietly = TRUE),
  require("dplyr", character.only = TRUE, quietly = TRUE)
)

if (require("CirceR", character.only = TRUE, quietly = TRUE)) {
  library(TreatmentPatterns)
  library(CDMConnector)
  library(dplyr)

  withr::local_envvar(
    R_USER_CACHE_DIR = tempfile(),
    EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
  )

  tryCatch(
    {
      if (Sys.getenv("skip_eunomia_download_test") != "TRUE") {
        CDMConnector::downloadEunomiaData(overwrite = TRUE)
      }
    },
    error = function(e) NA
  )

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

  executeTreatmentPatterns(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
}
#> 
#> Download completed!
#> Creating CDM database /tmp/Rtmpczt2kT/file29be1b75aef1/GiBleed_5.3.zip
#> ℹ Generating 8 cohorts
#> ℹ Generating cohort (1/8) - acetaminophen
#> ✔ Generating cohort (1/8) - acetaminophen [195ms]
#> 
#> ℹ Generating cohort (2/8) - amoxicillin
#> ✔ Generating cohort (2/8) - amoxicillin [155ms]
#> 
#> ℹ Generating cohort (3/8) - aspirin
#> ✔ Generating cohort (3/8) - aspirin [159ms]
#> 
#> ℹ Generating cohort (4/8) - clavulanate
#> ✔ Generating cohort (4/8) - clavulanate [158ms]
#> 
#> ℹ Generating cohort (5/8) - death
#> ✔ Generating cohort (5/8) - death [123ms]
#> 
#> ℹ Generating cohort (6/8) - doxylamine
#> ✔ Generating cohort (6/8) - doxylamine [148ms]
#> 
#> ℹ Generating cohort (7/8) - penicillinv
#> ✔ Generating cohort (7/8) - penicillinv [157ms]
#> 
#> ℹ Generating cohort (8/8) - viralsinusitis
#> ✔ Generating cohort (8/8) - viralsinusitis [233ms]
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
#> Censoring 1540 pathways with a frequency <5 to mean.
# }
```
