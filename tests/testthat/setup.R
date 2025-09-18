if (Sys.getenv("EUNOMIA_DATA_FOLDER", "") == "") {
  Sys.setenv("EUNOMIA_DATA_FOLDER" = tempfile("eunomiaData"))
  dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))

  CDMConnector::downloadEunomiaData()
  Eunomia::downloadEunomiaData(datasetName = "GiBleed")

  withr::defer(
    {
      unlink(Sys.getenv("EUNOMIA_DATA_FOLDER"), recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}
