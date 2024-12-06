#' getResultsDataModelSpecifications
#'
#' Gets the results data model specifications of `TreatmentPatterns`.
#'
#' @return `data.frame`
#' @export
#'
#' @examples{
#' getResultsDataModelSpecifications()
#' }
getResultsDataModelSpecifications <- function() {
  read.csv(system.file(package = "TreatmentPatterns", "resultsDataModelSpecifications.csv"), sep = ";")
}
