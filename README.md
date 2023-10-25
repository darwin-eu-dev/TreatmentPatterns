
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TreatmentPatterns

<!-- badges: start -->

[![R-CMD-check](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/R-CMD-check.yaml/badge.svg?branch=dev)](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/R-CMD-check.yaml)

![CRAN](https://www.r-pkg.org/badges/version/TreatmentPatterns)
[![Codecov test
coverage](https://codecov.io/gh/darwin-eu-dev/TreatmentPatterns/branch/master/graph/badge.svg)](https://app.codecov.io/gh/darwin-eu-dev/TreatmentPatterns?branch=master)
<!-- badges: end -->

This R package contains the resources for performing a treatment pathway
analysis of a study population of interest in observational databases.
The package partially relies on the Observational Medical Outcomes
Partnership Common Data Model (OMOP CDM), but the main parts of the
package are also usable with different data formats.

## Features

- Compatible with JSON, SQL, or [`CapR`](https://ohdsi.github.io/Capr/)
  cohorts.
- Compatible with
  [`DatabaseConnector`](https://ohdsi.github.io/DatabaseConnector/),
  [`CohortGenerator`](https://ohdsi.github.io/CohortGenerator/), and
  [`CDMConnector`](https://darwin-eu.github.io/CDMConnector/).
- Stratification by **age**, **sex**, and **index year**.
- Treatment type agnostic.
- Full control over treatment pathway definition:
  1.  Duration of treatments
  2.  Overlap of treatments
  3.  Gaps between treatments
- Intermediate patient level results can be reviewed, aggregate data can
  be shared.
- Easily integrate Sankey diagrams and sunburst plots (`htmlWidget`)
  into ShinyApps or web-pages.

## Installation

You can install the most recently released CRAN version of
TreatmentPatterns with:

``` r
install.packages("TreatmentPatterns")
```

Or from GitHub with:

``` r
remotes::install_github("darwin-eu-dev/TreatmentPatterns")
```

You can install the development version of TreatmentPatterns from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/TreatmentPatterns@dev")
```
