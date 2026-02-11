# Changelog

## \# TreatmentPatterns 3.1.2

- Fixes run-to-run stability of TreatmentPatterns.
- Fixes issue for andromeda \>= 1.0.0
- Fixed issue where doubles casted to numeric were reported in
  scientific notation.
- Fixed issue when parallelizing multiple computePathways(), caused by
  database-side conflicts.
- Fixed issue where the same subject_id was assigned a different
  pseudo-subject_id when using multiple cohort tables.
- Fixed issue when multiple target cohorts and multiple cohort tables
  were specified.
- Added export of problematic records when combination check fails.
- Deprecated SQL implementation for fetching cohort information.
- Internal change to always use CDMConnector internally (either using
  ODBC or JDBC).
- Require CDMConnector \>= 2.0.0.
- Gracefully exit when there is no internet connection when running
  internal helper functions for testing.

## \# TreatmentPatterns 3.1.1

CRAN release: 2025-08-25

- Fixed issue when setting `filterTreatments = "Changes"` and made it
  ambiguous regardless of the Andromeda version.
- Fixed issue where combination treatments were ordered differently when
  using Andromeda \>= 1.0.0.
- Fixed compatibility with Spark when using CDMConnector with ODBC.
- Updated the way data is copied from the `cdm_reference` to
  `andromeda`.

## \# TreatmentPatterns 3.1.0

CRAN release: 2025-06-03

- Added
  [`exportPatientLevel()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/exportPatientLevel.md)
  function to export patient level data.
- Added `startAnchor` parameter to dictate the window of interest.
- Added `windowStart` parameter to dictate the window of interest.
- Added `endAnchor` parameter to dictate the window of interest.
- Added `windowEnd` parameter to dictate the window of interest.
- Added `concatTargets` parameter to specify if multiple target cohorts
  should be treated separately, or continuously.
- Added `overlapMethod` parameter to pick a method how to deal with
  non-combination overlap between events.
- Removed `indexOffset` parameter.
- Removed `includeTreatments` parameter.
- Disabled failing test on CRAN environment
- Significantly reduced tests that run on CRAN.

## \# TreatmentPatterns 3.0.3

CRAN release: 2025-04-16

- Added compatibility with Andromeda 0.6.7 and 1.0.0

## \# TreatmentPatterns 3.0.2

CRAN release: 2025-04-04

- Added unit-testing for `CDMInterface` on Postgres, Redshift, SQL
  Server, Oracle, BigQuery, Spark, Snowflake
- Fixed some SQL issues for specific databases
- Fixed issue where a crash occurred on R 4.2.x, when saving the input
  arguments.
- Uncapped pathway length from 5.
- Fixed edge case on long repeating events to collapse.

## \# TreatmentPatterns 3.0.1

CRAN release: 2025-03-10

- Deprecated shiny app
- Removed shiny-related dependencies
- Added ggplot2 version of sunburst plot
- Fixed failing examples

## \# TreatmentPatterns 3.0.0

CRAN release: 2025-02-06

- Save parameters from
  [`computePathways()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/computePathways.md)
  and
  [`export()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md)
- A `TreatmentPatternsResults` (TPR) object is now returned from
  [`export()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md)
- Updated output file and column names to be snake_case rather than
  camelCase
  - The TPR object houses all the different tables, and has methods for
    plotting.
  - Writing CSV-files on
    [`export()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md)
    is now optional (`export(outputPath = "some/path/")`)
  - The TPR object has the following methods: `saveAsCsv()`,
    `saveAsZip()`, `uploadResultsToDb()`,
    [`load()`](https://rdrr.io/r/base/load.html), `plotSunburst()`,
    `plotSankey()`,
    [`plotEventDuration()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/plotEventDuration.md)
  - The TPR object has the following fields:, `attrition`, `metadata`,
    `treatment_pathways`, `summary_event_duration`, `counts_age`,
    `counts_sex`, `counts_year`, `cdm_source_info`, `analyses`,
    `arguments`
- Added `ResultModelManager` support
- Multiple `target` cohorts are now supported
- Allow `target`, `event`, and `exit` cohorts to be in different tables
- Stratification is now optional `export(stratify = TRUE)`
- When data is pulled from the database, subjects are assigned a new
  subject ID. This prevents rounding subject ID’s, and unintentional
  grouping of subjects.

## \# TreatmentPatterns 2.7.0

CRAN release: 2024-11-27

- Converted subject_id’s to pseudo_subject_id’s when pulling in data
  from the database. Actual subject_id’s are stored as
  [`character()`](https://rdrr.io/r/base/character.html).

- Added copyright statements in R-files.

- Renamed `periodPriorToIndex` to `indexDateOffset`.

- Added life cycle badge to ReadMe

- Added more informative error / warning messaging when no data is
  available to compute pathways.

- Extended output for event duration in pathways.

- Renamed `summaryStatsTherapyDuration.csv` to
  `summaryEventDuration.csv`.

- Updated status messaging in the console.

- Added `attrition.csv` that capture status messaging and time stamps.

- ## \# TreatmentPatterns 2.6.9

  CRAN release: 2024-11-27

- Disabled tests when required suggested packages are not available.

- Updated examples to only run when required suggested packages are
  available.

## \# TreatmentPatterns 2.6.8

CRAN release: 2024-08-29

- Updated some tests to work with later versions of omopgenerics.
- Fixed issue with where combinations sometimes got miss-classified.
- Fixed issue when event starts and ends on end-date of target.
- Fixed issue when collapsing events when there is also a combination,
  when `filterTreatments = "All"`.
- Added check in tests to only run if packages are availible.
  (noSuggests, M1).
- [`createSankeyDiagram()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSankeyDiagram.md)
  now supports pathways over 3 levels long.

## \# TreatmentPatterns 2.6.7

CRAN release: 2024-05-24

- Updated URLs in description
- Updated examples to fix compatibility with CDMConnector \>1.4.0
- Updated so more tests run on CRAN

## \# TreatmentPatterns 2.6.6

CRAN release: 2024-04-16

- Internal update to shinyApp.
- Update to createSankeyDiagram() to properly handle combinations
  consisting of \>2.
- Added article to package website about best practices.

## \# TreatmentPatterns 2.6.5

CRAN release: 2024-02-19

- Removed stringi, rjson, and googleVis as dependencies.
- Fixed unit tests that had dummy data outside of observation data.
- Fix for complex edge-case paths with re-occuring treatments.
- Added unit tests for edge-cases.
- Added warning when `minEraDuration` \> `minPostCombinationWindow` or
  `combinationWindow`.
- Fixed issue when `filterTreatments` was set to `"changes"`, age and
  sex columns were dropped from output.
- datatypes of cohort_table are now checked.

## \# TreatmentPatterns 2.6.4

CRAN release: 2024-01-26

- Fixed issue with minPostCombinationWindow broken by the fix for
  re-occurring treatments.
- Added unit tests for minPostCombinationWindow.

## \# TreatmentPatterns 2.6.3

CRAN release: 2024-01-25

- Fixed issue where there were duplicate rows in the exported
  treatmentPathways.csv file.
- Added more comprehensive description for parameter `ageWindow` for
  [`export()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md).
- Added additional tests validating the counts per pathway being
  exported by
  [`export()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md).
- Fixed issue with re-occurring treatments in pathways.
- Added dedicated logical tests for various pathways.
- Added dedicated tests for target-event cohort overlaps.
- Fixed spelling error in outputs, and interaction with them in shiny
  module.

## \# TreatmentPatterns 2.6.2

CRAN release: 2023-12-13

- Fixed spelling mistake ‘summaryStatsTherapyDuraion’ to
  ‘summaryStatsTherapyDuration’.
- Added error when only one level of data is available in the data when
  computing a sankey diagram.
- Removed `createSankeyDiagram`
- Renamed `createSankeyDiagram2` to `createSankeyDiagram`
- Removed `createSunburstPlot`
- Renamed `createSunburstPlot2` to `createSunburstPlot`
- Fixed issue where target cohorts had to be at least 1 day long to be
  included when using DatabaseConnector.
- Fixed CRAN error when running examples.
- Renamed `cellCount` to `minCellCount` in export documentation.
- Fixed issue where counts of pathways were not computed correctly.
- Fixed spelling error `summaryStatsTherapyDuraion` to
  `summaryStatsTherapyDuration`.
- Fixed issue in `export` where merging two tables could fail, if one
  was empty.

## \# TreatmentPatterns 2.6.1

CRAN release: 2023-12-11

- Bumped R version to 4.2.1
- Added additional unit-tests for `createSunburstPlot2()` and
  `createSankeyDiagram2()`
- Updated to use DATEDIFF to compute difference between dates when
  fetching data from the database.
- Added functionality to choose how groups below the minimum cell count
  are censored.
- Made some parameters ambiguous for `createSankeyDiagram2` and
  `createSunburstPlot2()` for more control over the figures.
- Removed `addNoPath` parameter.
- Renamed `minFreq` parameter to `minCellCount`.
- Added Shiny app with exported module classes.
- Added censoring options of treatment pathways falling below the
  `minCellCount`.
- Simplified part of
  [`computePathways()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/computePathways.md).
- Significantly improved performance of
  [`export()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md).

## \# TreatmentPatterns 2.6.0

CRAN release: 2023-11-13

- Added `createSunburstPlot2()` as a replacement of
  [`createSunburstPlot()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSunburstPlot.md).
  `createSunburstPlot2()` will fully replace
  [`createSunburstPlot()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSunburstPlot.md)
  in a future version.
- Moved `DatabaseConnector` and `SqlRender` to Suggests
- Removed obsolete dependencies: `data.table`, `fs`, and `glue`.
- Internal performance updates.
- Internal code style updates.
- Moved basic filtering when fetching cohort table.
- Updated ReadMe with functionalities
- Internal updates to LRFS paths, to allow paths of identical duration.

## \# TreatmentPatterns 2.5.2

CRAN release: 2023-09-15

- Resolved issue with finalize method of CDMInterface class.
- Resolved issue with schema references.
- Unified case style.
- Refactored code for
  [`createSunburstPlot()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSunburstPlot.md).

## \# TreatmentPatterns 2.5.1

CRAN release: 2023-09-07

- Added checks for user input: `cohorts` and `cohortTableName`.
- Added option to directly return HTML when using `createSankeyDiagram`.
- Added option to directly return HTML when using `createSunburstPlot`.
- Added option for ageWindow to be a vector.
- Added input checking for
  [`export()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/export.md).
- Added additional check for frequency for `createSunburstPlot`.
- Resolved issue in `createSunburstPlot`, when converting from
  data.table to nested JSON.

## \# TreatmentPatterns 2.5.0

CRAN release: 2023-08-16

- Updated interface
- Some internal OO usage
- CDM & DatabaseConnector
- Uses Andromeda to be able to handle bigger than RAM data sets.
  - Shift from data.table to dplyr.
- Updated vignettes using new interface
- General code clean up
- Intermediate files are cached and accessable through Andromeda for
  review.
- Outputted CSV-files re-imagined to be more flexible for use post
  TreatmentPatterns.
- Sunburst and Sankey plots are now directly usable with
  treatmentPathways.csv.
