# exportPatientLevel

Exports patient-level files for custom data analysis.

## Usage

``` r
exportPatientLevel(andromeda, outputPath)
```

## Arguments

- andromeda:

  (`Andromeda`) Andromeda object from
  [`computePathways()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/computePathways.md).

- outputPath:

  (`character(1)`) Directory where to write output files to.

## Value

`NULL`

## Examples

``` r
if (interactive()) {
  library(CDMConnector)
  library(DBI)
  library(TreatmentPatterns)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomiaDir())
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

  exportPatientLevel(outputEnv, tempdir())
}
```
