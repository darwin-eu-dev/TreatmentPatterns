# createSunburstPlot

New sunburstPlot function

## Usage

``` r
createSunburstPlot(treatmentPathways, groupCombinations = FALSE, ...)
```

## Arguments

- treatmentPathways:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  The contents of the treatmentPathways.csv-file as a data.frame().

- groupCombinations:

  (`logical(1)`: `FALSE`)  

  `TRUE`

  :   Group all combination treatments in category `"Combination"`.

  `FALSE`

  :   Do not group combination treatments.

- ...:

  Paramaters for
  [sunburst](https://rdrr.io/pkg/sunburstR/man/sunburst.html).

## Value

(`htmlwidget`)

## Examples

``` r
# Dummy data, typically read from treatmentPathways.csv
treatmentPatwhays <- data.frame(
  pathway = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
           "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
  freq = c(206, 6, 14, 48, 221),
  sex = rep("all", 5),
  age = rep("all", 5),
  index_year = rep("all", 5)
)

if (interactive()) {
  createSunburstPlot(treatmentPatwhays)
}
```
