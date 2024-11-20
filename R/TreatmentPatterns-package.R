#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import checkmate
#' @import stringr
#' @import utils
#' @import dplyr
#' @import Andromeda
#' @import R6
#' @import sunburstR
#' @import shiny
#' @import shinydashboard
#' @importFrom networkD3 sankeyNetwork
#' @importFrom htmlwidgets JS
#' @importFrom stats sd median quantile
#' @importFrom tidyr pivot_wider
## usethis namespace: end
NULL

# Global Variables
utils::globalVariables(
  c(
    "x",
    "y",
    "rowNumber",
    "path",
    "personId",
    "freq",
    "Q1",
    "Q2",
    "eventName"
  )
)
