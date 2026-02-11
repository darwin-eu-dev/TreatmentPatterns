# TreatmentPatternsResults Class

Houses the results of a `TreatmentPatterns` analysis. Each field
corresponds to a file. Plotting methods are provided.

## Active bindings

- `attrition`:

  (`data.frame`)

- `metadata`:

  (`data.frame`)

- `treatment_pathways`:

  (`data.frame`)

- `summary_event_duration`:

  (`data.frame`)

- `counts_age`:

  (`data.frame`)

- `counts_sex`:

  (`data.frame`)

- `counts_year`:

  (`data.frame`)

- `cdm_source_info`:

  (`data.frame`)

- `analyses`:

  (`data.frame`)

- `arguments`:

  (`list`)

## Methods

### Public methods

- [`TreatmentPatternsResults$new()`](#method-TreatmentPatternsResults-new)

- [`TreatmentPatternsResults$saveAsZip()`](#method-TreatmentPatternsResults-saveAsZip)

- [`TreatmentPatternsResults$saveAsCsv()`](#method-TreatmentPatternsResults-saveAsCsv)

- [`TreatmentPatternsResults$uploadResultsToDb()`](#method-TreatmentPatternsResults-uploadResultsToDb)

- [`TreatmentPatternsResults$load()`](#method-TreatmentPatternsResults-load)

- [`TreatmentPatternsResults$plotSunburst()`](#method-TreatmentPatternsResults-plotSunburst)

- [`TreatmentPatternsResults$plotSankey()`](#method-TreatmentPatternsResults-plotSankey)

- [`TreatmentPatternsResults$plotEventDuration()`](#method-TreatmentPatternsResults-plotEventDuration)

- [`TreatmentPatternsResults$clone()`](#method-TreatmentPatternsResults-clone)

------------------------------------------------------------------------

### Method `new()`

Initializer method

#### Usage

    TreatmentPatternsResults$new(
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
      filePath = NULL
    )

#### Arguments

- `attrition`:

  (`data.frame`) attrition result.

- `metadata`:

  (`data.frame)`) metadata result.

- `treatmentPathways`:

  (`data.frame)`) treatmentPathways result.

- `summaryEventDuration`:

  (`data.frame)`) summaryEventDuration result.

- `countsAge`:

  (`data.frame)`) countsAge result.

- `countsSex`:

  (`data.frame)`) countsSex result.

- `countsYear`:

  (`data.frame)`) countsYear result.

- `cdmSourceInfo`:

  (`data.frame`) cdmSourceInfo result.

- `analyses`:

  (`data.frame`) Analyses result.

- `arguments`:

  (`list`) Named list of arguments used.

- `filePath`:

  (`character`) File path to either a directory or zip-file, containing
  the csv-files.

------------------------------------------------------------------------

### Method `saveAsZip()`

Save the results as a zip-file.

#### Usage

    TreatmentPatternsResults$saveAsZip(path, name, verbose = TRUE)

#### Arguments

- `path`:

  (`character(1)`) Path to write to.

- `name`:

  (`character(1)`) File name.

- `verbose`:

  (`logical`: `TRUE`) Verbose messaging.

#### Returns

`self`

------------------------------------------------------------------------

### Method `saveAsCsv()`

Save the results as csv-files.

#### Usage

    TreatmentPatternsResults$saveAsCsv(path, verbose = TRUE)

#### Arguments

- `path`:

  (`character(1)`) Path to write to.

- `verbose`:

  (`logical`: `TRUE`) Verbose messaging.

#### Returns

`self`

------------------------------------------------------------------------

### Method `uploadResultsToDb()`

Upload results to a resultsDatabase using `ResultModelManager`.

#### Usage

    TreatmentPatternsResults$uploadResultsToDb(
      connectionDetails,
      schema,
      prefix = "tp_",
      overwrite = TRUE,
      purgeSiteDataBeforeUploading = FALSE
    )

#### Arguments

- `connectionDetails`:

  (`ConnectionDetails`) ConnectionDetails object from
  `DatabaseConnector`.

- `schema`:

  (`character(1)`) Schema to write tables to.

- `prefix`:

  (`character(1)`: `"tp_"`) Table prefix.

- `overwrite`:

  (`logical(1)`: `TRUE`) Should tables be overwritten?

- `purgeSiteDataBeforeUploading`:

  (`logical`: `FALSE`) Should site data be purged before uploading?

#### Returns

`self`

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load data from files.

#### Usage

    TreatmentPatternsResults$load(filePath)

#### Arguments

- `filePath`:

  (`character(1)`) Path to a directory or zip-file containing the result
  csv-files.

#### Returns

`self`

------------------------------------------------------------------------

### Method `plotSunburst()`

Wrapper for
[`TreatmentPatterns::createSunburstPlot()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSunburstPlot.md),
but with data filtering step.

#### Usage

    TreatmentPatternsResults$plotSunburst(
      age = "all",
      sex = "all",
      indexYear = "all",
      nonePaths = FALSE,
      ...
    )

#### Arguments

- `age`:

  (`character(1)`) Age group.

- `sex`:

  (`character(1)`) Sex group.

- `indexYear`:

  (`character(1)`) Index year group.

- `nonePaths`:

  (`logical(1)`) Should `None` paths be included?

- `...`:

  Parameters for
  [`TreatmentPatterns::createSunburstPlot()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSunburstPlot.md)

#### Returns

`htmlwidget`

------------------------------------------------------------------------

### Method `plotSankey()`

Wrapper for
[`TreatmentPatterns::createSankeyDiagram()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSankeyDiagram.md),
but with data filtering step.

#### Usage

    TreatmentPatternsResults$plotSankey(
      age = "all",
      sex = "all",
      indexYear = "all",
      nonePaths = FALSE,
      ...
    )

#### Arguments

- `age`:

  (`character(1)`) Age group.

- `sex`:

  (`character(1)`) Sex group.

- `indexYear`:

  (`character(1)`) Index year group.

- `nonePaths`:

  (`logical(1)`) Should `None` paths be included?

- `...`:

  Parameters for
  [`TreatmentPatterns::createSankeyDiagram()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/createSankeyDiagram.md)

#### Returns

`htmlwidget`

------------------------------------------------------------------------

### Method [`plotEventDuration()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/plotEventDuration.md)

Wrapper for
[`TreatmentPatterns::plotEventDuration()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/plotEventDuration.md).

#### Usage

    TreatmentPatternsResults$plotEventDuration(...)

#### Arguments

- `...`:

  Parameters for
  [`TreatmentPatterns::plotEventDuration()`](https://darwin-eu-dev.github.io/TreatmentPatterns/reference/plotEventDuration.md)

#### Returns

`ggplot`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TreatmentPatternsResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
