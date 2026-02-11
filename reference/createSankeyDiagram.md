# createSankeyDiagram

Create sankey diagram.

## Usage

``` r
createSankeyDiagram(
  treatmentPathways,
  groupCombinations = FALSE,
  colors = NULL,
  ...
)
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

- colors:

  (`character(n)`) Vector of hex color codes.

- ...:

  Paramaters for
  [sankeyNetwork](https://rdrr.io/pkg/networkD3/man/sankeyNetwork.html).

## Value

(`htmlwidget`)

## Examples

``` r
# Dummy data, typically read from treatmentPathways.csv
treatmentPathways <- data.frame(
  pathway = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
           "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
  freq = c(206, 6, 14, 48, 221),
  sex = rep("all", 5),
  age = rep("all", 5),
  index_year = rep("all", 5)
)

if (interactive()) {
  createSankeyDiagram(treatmentPathways)
}
```
