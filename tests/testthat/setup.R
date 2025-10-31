require("withr", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

if (Sys.getenv("EUNOMIA_DATA_FOLDER_CG") == "") {
  Sys.setenv("EUNOMIA_DATA_FOLDER_CG" = tempfile("eunomiaData_CG"))
  dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER_CG"))

  if (require("Eunomia", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
    Eunomia::downloadEunomiaData(datasetName = "GiBleed", pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER_CG"))
    Eunomia::extractLoadData(
      from = file.path(Sys.getenv("EUNOMIA_DATA_FOLDER_CG"), "GiBleed_5.3.zip"),
      to = file.path(Sys.getenv("EUNOMIA_DATA_FOLDER_CG"), "eunomia.sqlite")
    )
    .CM <- generateCohortTableCDMC()
  }

  withr::defer(
    {
      unlink(Sys.getenv("EUNOMIA_DATA_FOLDER_CG"), recursive = TRUE, force = TRUE)
    }
  )
} else {
  .CM <- generateCohortTableCDMC()
}

if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
  Sys.setenv("EUNOMIA_DATA_FOLDER" = tempfile("eunomiaData"))
  dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))

  if (require("CDMConnector", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
    CDMConnector::downloadEunomiaData(overwrite = TRUE)
    .CG <- generateCohortTableCG()
  }

  withr::defer(
    {
      unlink(Sys.getenv("EUNOMIA_DATA_FOLDER"), recursive = TRUE, force = TRUE)
    }
  )
} else {
  .CG <- generateCohortTableCG()
}
