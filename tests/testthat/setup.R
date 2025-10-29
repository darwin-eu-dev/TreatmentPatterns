if (Sys.getenv("EUNOMIA_DATA_FOLDER", "") == "") {
  Sys.setenv("EUNOMIA_DATA_FOLDER" = tempfile("eunomiaData"))
  dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))

  if (require("CDMConnector", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
    CDMConnector::downloadEunomiaData(overwrite = TRUE)
  }

  withr::defer(
    {
      unlink(Sys.getenv("EUNOMIA_DATA_FOLDER"), recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}
