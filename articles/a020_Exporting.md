# Exporing Results

## Exporting result objects

The `export` function allows us to export the generated result objects
from `computePathways`. There are several arguments that we can change
to alter the behavior, depending on what we are allowed to share.

### minCellCount and censorType

Let’s say we are only able to share results of groups of subjects that
have at least 5 subjects in them.

``` r
results <- export(
  andromeda = defaultSettings,
  minCellCount = 5
)
```

We can also choose between different methods how to handle pathways that
fall below are specified `minCellCount`. These types are **1**)
`"cellCount"`, **2**) `"remove"`, and **3**) `"mean"`.

We could say we want to censor all pathways that fall below the
`minCellCount` to be censored *to* the `minCellCount`.

``` r
resultsA <- export(
  andromeda = minEra60,
  minCellCount = 5,
  censorType = "minCellCount"
)
```

Or we could completely remove them

``` r
resultsB <- export(
  andromeda = minEra60,
  minCellCount = 5,
  censorType = "remove"
)
```

Or finally we can censor them as the mean of all the groups that fall
below the `minCellCount`.

``` r
resultsC <- export(
  andromeda = minEra60,
  minCellCount = 5,
  censorType = "mean"
)
```

### ageWindow

We can also specify an age window.

``` r
resultsD <- export(
  andromeda = splitAcuteTherapy,
  minCellCount = 5,
  censorType = "mean",
  ageWindow = 3
)
```

Or a collection of ages.

``` r
resultsE <- export(
  andromeda = splitAcuteTherapy,
  minCellCount = 5,
  censorType = "mean",
  ageWindow = c(0, 18, 25, 30, 40, 50, 60, 150)
)
```

### archiveName

Finally we can also specify an `archiveName` which is the name of a
zip-file to zip all our output csv-files to.

``` r
resultsF <- export(
  andromeda = includeEndDate,
  minCellCount = 5,
  censorType = "mean",
  ageWindow = 3,
  archiveName = "output.zip"
)
```

## Patient-Level Export

We can also export **patient-level** data to use for internal analyses.
Obviously these results are not share-able. Currently the results are
only exported as csv-files.

``` r
exportPatientLevel(
  andromeda = outputEnv,
  outputPath = tempdir()
)
```

We go into evaluating the output of the files in the **Evaluating
Output** vignette.
