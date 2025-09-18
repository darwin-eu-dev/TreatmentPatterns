if (Sys.getenv("EUNOMIA_DATA_FOLDER", "") == "") {
  Sys.setenv("EUNOMIA_DATA_FOLDER" = tempfile("eunomiaData"))
  dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))

  if (Sys.getenv("EUNOMIA_DATA_FOLDER") |> list.files() |> length() == 0) {
    CDMConnector::downloadEunomiaData()
    Eunomia::downloadEunomiaData(datasetName = "GiBleed")
  }

  withr::defer(
    {
      unlink(Sys.getenv("EUNOMIA_DATA_FOLDER"), recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}
