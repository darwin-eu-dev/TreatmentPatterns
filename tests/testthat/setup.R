require("withr", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)

# if (Sys.getenv("EUNOMIA_DATA_FOLDER_CG") == "") {
#   Sys.setenv("EUNOMIA_DATA_FOLDER_CG" = tempfile("eunomiaData_CG"))
#   dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER_CG"))
# 
#   if (
#     require("Eunomia", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
#     & require("DatabaseConnector", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
#     & require("SqlRender", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
#     & require("CirceR", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)
#   ) {
#     Eunomia::getDatabaseFile(
#       datasetName = "GiBleed",
#       pathToData = Sys.getenv("EUNOMIA_DATA_FOLDER_CG"),
#       databaseFile = file.path(Sys.getenv("EUNOMIA_DATA_FOLDER_CG"), "GiBleed_5.3.sqlite")
#     )
#     .CG <- generateCohortTableCG()
#   }
# 
#   withr::defer(
#     {
#       unlink(Sys.getenv("EUNOMIA_DATA_FOLDER_CG"), recursive = TRUE, force = TRUE)
#     }
#   )
# } else {
#   .CG <- generateCohortTableCG()
# }

if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
  Sys.setenv("EUNOMIA_DATA_FOLDER" = file.path(tempdir(), "eunomiaData"))
  dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))

  if (require("CDMConnector", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
    CDMConnector::downloadEunomiaData(overwrite = TRUE)
    .CM <- generateCohortTableCDMC()
  }

  # withr::defer(
  #   {
  #     unlink(Sys.getenv("EUNOMIA_DATA_FOLDER"), recursive = TRUE, force = TRUE)
  #   }
  # )
} else {
  .CM <- generateCohortTableCDMC()
}
