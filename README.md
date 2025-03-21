
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TreatmentPatterns

<!-- badges: start -->

[![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/R-CMD-check.yaml/badge.svg?branch=dev)](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/R-CMD-check.yaml)
[![CRAN](https://www.r-pkg.org/badges/version/TreatmentPatterns)](https://CRAN.R-project.org/package=TreatmentPatterns)
[![Codecov test
coverage](https://codecov.io/gh/darwin-eu-dev/TreatmentPatterns/branch/master/graph/badge.svg)](https://app.codecov.io/gh/darwin-eu-dev/TreatmentPatterns?branch=master)

![PostgreSQL](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-postgresql.yaml/badge.svg)
![SQL
Server](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-sqlserver.yaml/badge.svg)
![Snowflake](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-snowflake.yaml/badge.svg)
![Oracle](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-oracle.yaml/badge.svg)
![Redshift](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-redshift.yaml/badge.svg)
![Spark](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-Spark/badge.svg)
![Iris](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-iris.yaml/badge.svg)
![BigQuery](https://github.com/darwin-eu-dev/TreatmentPatterns/actions/workflows/db-bigquery.yaml/badge.svg)

<!-- badges: end -->

[*Markus A, Verhamme K, Kors J, Rijnbeek P (2022). “TreatmentPatterns:
An R package to facilitate the standardized development and analysis of
treatment patterns across disease domains.” Computer Methods and
Programs in Biomedicine.*](https://doi.org/10.1016/j.cmpb.2022.107081)

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
