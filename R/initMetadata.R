initMetadata <- function(andromeda) {
  andromeda$metadata <- data.frame(
    execution_start = Sys.time(),
    package_version = as.character(utils::packageVersion("TreatmentPatterns")),
    r_version = base::version$version.string,
    platform = base::version$platform
  )
  return(invisible(andromeda))
}