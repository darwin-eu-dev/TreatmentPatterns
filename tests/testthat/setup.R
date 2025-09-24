# if (Sys.getenv("EUNOMIA_DATA_FOLDER") == "") {
#   Sys.setenv("EUNOMIA_DATA_FOLDER" = tempfile("eunomiaData"))
#   dir.create(Sys.getenv("EUNOMIA_DATA_FOLDER"))
# 
#   if (require("CDMConnector", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
#     CDMConnector::downloadEunomiaData()
#   }
# 
#   if (require("Eunomia", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
#     Eunomia::downloadEunomiaData(datasetName = "GiBleed")
#   }
# 
#   if (require("withr", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
#     withr::defer(
#     {
#       unlink(Sys.getenv("EUNOMIA_DATA_FOLDER"), recursive = TRUE, force = TRUE)
#     },
#     if (require("testthat", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
#       testthat::teardown_env()
#     }
#   )
#   }
# }

withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env(),
  EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
)
