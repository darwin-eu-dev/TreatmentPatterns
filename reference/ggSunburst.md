# ggSunburst

ggSunburst

## Usage

``` r
ggSunburst(treatmentPathways, groupCombinations = FALSE, unit = "percent")
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

- unit:

  (`character(1)`) Either `"count"` or `"percent"`, to scale the plot
  to.

## Value

(`gg`, `ggplot`)

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
  ggSunburst(treatmentPatwhays)
}
```
