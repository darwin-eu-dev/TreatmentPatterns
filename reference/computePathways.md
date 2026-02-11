# computePathways

Compute treatment patterns according to the specified parameters within
specified cohorts.

## Usage

``` r
computePathways(
  cohorts,
  cohortTableName,
  cdm = NULL,
  connectionDetails = NULL,
  cdmSchema = NULL,
  resultSchema = NULL,
  analysisId = 1,
  description = "",
  tempEmulationSchema = NULL,
  startAnchor = "startDate",
  windowStart = 0,
  endAnchor = "endDate",
  windowEnd = 0,
  minEraDuration = 0,
  splitEventCohorts = NULL,
  splitTime = NULL,
  eraCollapseSize = 30,
  combinationWindow = 30,
  minPostCombinationDuration = 30,
  filterTreatments = "First",
  maxPathLength = 5,
  overlapMethod = "truncate",
  concatTargets = TRUE
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

- analysisId:

  (`character(1)`) Identifier for the TreatmentPatterns analysis.

- description:

  (`character(1)`) Description of the analysis.

- tempEmulationSchema:

  Schema used to emulate temp tables

- startAnchor:

  (`character(1)`: `"startDate"`) Start date anchor. One of:
  `"startDate"`, `"endDate"`

- windowStart:

  (`numeric(1)`: `0`) Offset for `startAnchor` in days.

- endAnchor:

  (`character(1)`: `"endDate"`) End date anchor. One of: `"startDate"`,
  `"endDate"`

- windowEnd:

  (`numeric(1)`: `0`) Offset for `endAnchor` in days.

- minEraDuration:

  (`integer(1)`: `0`)  
  Minimum time an event era should last to be included in analysis

- splitEventCohorts:

  (`character(n)`: `""`)  
  Specify event cohort to split in acute (\< X days) and therapy (\>= X
  days)

- splitTime:

  (`integer(1)`: `30`)  
  Specify number of days (X) at which each of the split event cohorts
  should be split in acute and therapy

- eraCollapseSize:

  (`integer(1)`: `30`)  
  Window of time between which two eras of the same event cohort are
  collapsed into one era

- combinationWindow:

  (`integer(1)`: `30`)  
  Window of time two event cohorts need to overlap to be considered a
  combination treatment

- minPostCombinationDuration:

  (`integer(1)`: `30`)  
  Minimum time an event era before or after a generated combination
  treatment should last to be included in analysis

- filterTreatments:

  (`character(1)`: `"First"` \["first", "Changes", "all"\])  
  Select first occurrence of (‘First’); changes between (‘Changes’); or
  all event cohorts (‘All’).

- maxPathLength:

  (`integer(1)`: `5`)  
  Maximum number of steps included in treatment pathway

- overlapMethod:

  (`character(1)`: `"truncate"`) Method to decide how to deal with
  overlap that is not significant enough for combination. `"keep"` will
  keep the dates as is. `"truncate"` truncates the first occurring event
  to the start date of the next event.

- concatTargets:

  (`logical(1)`: `TRUE`) Should multiple target cohorts for the same
  person be concatenated or not?

## Value

([`Andromeda::andromeda()`](https://rdrr.io/pkg/Andromeda/man/andromeda_constructor.html))
[andromeda](https://rdrr.io/pkg/Andromeda/man/andromeda_constructor.html)
object containing non-sharable patient level data outcomes.

## Examples

``` r
# \donttest{
ableToRun <- all(
  require("CirceR", character.only = TRUE, quietly = TRUE),
  require("CDMConnector", character.only = TRUE, quietly = TRUE),
  require("TreatmentPatterns", character.only = TRUE, quietly = TRUE),
  require("dplyr", character.only = TRUE, quietly = TRUE)
)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

if (ableToRun) {
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

  outputEnv <- computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm
  )

  Andromeda::close(outputEnv)
  DBI::dbDisconnect(con, shutdown = TRUE)
}
#> 
#> Download completed!
#> Creating CDM database /tmp/RtmphA5SU8/file29704481b63b/GiBleed_5.3.zip
#> ℹ Generating 8 cohorts
#> ℹ Generating cohort (1/8) - acetaminophen
#> ✔ Generating cohort (1/8) - acetaminophen [310ms]
#> 
#> ℹ Generating cohort (2/8) - amoxicillin
#> ✔ Generating cohort (2/8) - amoxicillin [204ms]
#> 
#> ℹ Generating cohort (3/8) - aspirin
#> ✔ Generating cohort (3/8) - aspirin [176ms]
#> 
#> ℹ Generating cohort (4/8) - clavulanate
#> ✔ Generating cohort (4/8) - clavulanate [175ms]
#> 
#> ℹ Generating cohort (5/8) - death
#> ✔ Generating cohort (5/8) - death [134ms]
#> 
#> ℹ Generating cohort (6/8) - doxylamine
#> ✔ Generating cohort (6/8) - doxylamine [151ms]
#> 
#> ℹ Generating cohort (7/8) - penicillinv
#> ✔ Generating cohort (7/8) - penicillinv [154ms]
#> 
#> ℹ Generating cohort (8/8) - viralsinusitis
#> ✔ Generating cohort (8/8) - viralsinusitis [251ms]
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
