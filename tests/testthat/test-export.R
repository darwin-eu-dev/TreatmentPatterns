# General ----
library(TreatmentPatterns)
library(testthat)
library(dplyr)

# Tests ----
test_that("void", {
  expect_error(
    TreatmentPatterns::export()
  )
})

test_that("empty treatmentHistory table", {
  tempDirLocal <- file.path(tempdir(), "output")
  localAndromeda <- Andromeda::andromeda()
  
  localAndromeda$treatmentHistory <- data.frame(
    personId = numeric(0)
  )
  
  expect_message(
    export(localAndromeda, outputPath = tempDirLocal),
    "Treatment History table is empty. Nothing to export."
  )
})

# CohortGenerator ----
test_that("outputPath", {
  skip_if_not(ableToRun()$CG)
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema)
  
  ## file.path(tempDirCG) ----
  tempDirLocal <- file.path(tempdir(), "output")

  result <- export(andromeda, outputPath = tempDirLocal)

  expect_true(
    file.exists(file.path(tempDirLocal, "treatment_pathways.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "summary_event_duration.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "counts_year.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "counts_age.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "counts_sex.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "attrition.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "cdm_source_info.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "analyses.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "metadata.csv"))
  )

  ## 3 ----
  expect_error(
    TreatmentPatterns::export(
      andromeda,
      outputPath = 3,
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Variable 'outputPath':"
  )
  
  Andromeda::close(andromeda)
})

test_that("ageWindow", {
  skip_if_not(ableToRun()$CG)
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )

  ## 10 ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      ageWindow = 10,
      nonePaths = TRUE,
      stratify = TRUE
    )
  )

  treatmentPathways <- result$treatment_pathways

  expect_true(
    all(c("0-10", "10-20", "20-30", "30-40", "40-50", "all") %in% treatmentPathways$age))

  ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150),
      nonePaths = TRUE,
      stratify = TRUE
    )
  )

  treatmentPathways <- result$treatment_pathways

  expect_true(all(
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
      "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
  ))

  Andromeda::close(andromeda)
})

test_that("minCellCount", {
  skip_if_not(ableToRun()$CG)
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )

  ## 10 ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "remove",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Removing \\d+ pathways with a frequency <10."
  )

  treatmentPathways <- result$treatment_pathways

  expect_equal(min(treatmentPathways$freq), 10)

  ## "10" ----
  expect_error(
    export(
      andromeda = andromeda,
      minCellCount = "10",
      nonePaths = TRUE,
      stratify = TRUE
    )
  )
  
  Andromeda::close(andromeda)
})

test_that("archiveName", {
  skip_if_not(ableToRun()$CG)
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  ## "output.zip" ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = "output.zip"
    )
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "output.zip"))
  )

  ## 3 ----
  expect_error(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = 3,
      nonePaths = TRUE,
      stratify = TRUE
    )
  )
  
  Andromeda::close(andromeda)
})

test_that("censorType", {
  skip_if_not(ableToRun()$CG)
  
  globals <- generateCohortTableCG()
  
  andromeda <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )

  ## "remove" ----
  expect_message(
    result <- TreatmentPatterns::export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "remove",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Removing \\d+ pathways with a frequency <10."
  )

  treatmentPathways <- result$treatment_pathways

  expect_equal(min(treatmentPathways$freq), 10)

  ## "minCellCount" ----
  expect_message(
    TreatmentPatterns::export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "minCellCount",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Censoring \\d+ pathways with a frequency <10 to 10."
  )
  
  treatmentPathways <- result$treatment_pathways
  
  expect_equal(min(treatmentPathways$freq), 10)
  
  ## "mean" ----
  expect_message(
    result <- TreatmentPatterns::export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "mean",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Censoring \\d+ pathways with a frequency <10 to mean."
  )
  
  treatmentPathways <- result$treatment_pathways
  
  expect_equal(min(treatmentPathways$freq), 2)
  
  ## "stuff" ----
  expect_error(
    export(
      andromeda = andromeda,
      censorType = "Stuff",
      nonePaths = TRUE,
      stratify = TRUE
    )
  )

  Andromeda::close(andromeda)
})

# CDMConnector ----
test_that("outputPath", {
  skip_if_not(ableToRun()$CDMC)
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  result <- export(andromeda, outputPath = tempDirLocal)

  expect_true(
    file.exists(file.path(tempDirLocal, "treatment_pathways.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "summary_event_duration.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "counts_year.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "counts_age.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "counts_sex.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "attrition.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "cdm_source_info.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "analyses.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirLocal, "metadata.csv"))
  )

  ## 3 ----
  expect_error(
    export(
      andromeda,
      outputPath = 3,
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Variable 'outputPath':"
  )
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("ageWindow", {
  skip_if_not(ableToRun()$CDMC)
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )

  ## 10 ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      ageWindow = 10,
      nonePaths = TRUE,
      stratify = TRUE
    )
  )

  treatmentPathways <- result$treatment_pathways

  expect_true(all(c("0-10", "10-20", "all") %in% treatmentPathways$age))

  ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150),
      nonePaths = TRUE,
      stratify = TRUE
    )
  )

  treatmentPathways <- result$treatment_pathways

  expect_true(all(
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
      "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
  ))
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("minCellCount", {
  skip_if_not(ableToRun()$CDMC)
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )

  ## 10 ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "remove",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Removing \\d+ pathways with a frequency <10."
  )

  treatmentPathways <- result$treatment_pathways

  expect_equal(min(treatmentPathways$freq), 10)

  ## "10" ----
  expect_error(
    export(
      andromeda = andromeda,
      minCellCount = "10",
      nonePaths = TRUE,
      stratify = TRUE
    )
  )
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("archiveName", {
  skip_if_not(ableToRun()$CDMC)
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  tempDirLocal <- file.path(tempdir(), "output")

  ## "output.zip" ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = "output.zip",
      nonePaths = TRUE,
      stratify = TRUE
    )
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "output.zip"))
  )

  ## 3 ----
  expect_error(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = 3,
      nonePaths = TRUE,
      stratify = TRUE
    )
  )
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("censorType", {
  skip_if_not(ableToRun()$CDMC)
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )

  ## "remove" ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "remove",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Removing \\d+ pathways with a frequency <10."
  )
  
  treatmentPathways <- result$treatment_pathways

  expect_equal(min(treatmentPathways$freq), 10)

  ## "minCellCount" ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "minCellCount",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Censoring \\d+ pathways with a frequency <10 to 10."
  )
  
  treatmentPathways <- result$treatment_pathways

  expect_equal(min(treatmentPathways$freq), 10)

  ## "mean" ----
  expect_message(
    result <- export(
      andromeda = andromeda,
      minCellCount = 10,
      censorType = "mean",
      nonePaths = TRUE,
      stratify = TRUE
    ),
    "Censoring \\d+ pathways with a frequency <10 to mean."
  )
  
  treatmentPathways <- result$treatment_pathways
  
  expect_equal(min(treatmentPathways$freq), 2)
  
  ## "stuff" ----
  expect_error(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      censorType = "Stuff",
      nonePaths = TRUE,
      stratify = TRUE
    )
  )
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("counts", {
  skip_if_not(ableToRun()$CDMC)
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )

  ## "remove" ----
  result <- TreatmentPatterns::export(
    andromeda = andromeda,
    minCellCount = 1,
    ageWindow = c(0, 18, 150),
    nonePaths = TRUE,
    stratify = TRUE
  )

  treatmentPathways <- result$treatment_pathways

  totalAll <- treatmentPathways %>%
    dplyr::filter(.data$age == "all", .data$sex == "all", index_year == "all") %>%
    summarize(sum(freq)) %>%
    pull()
  
  # all == male + female
  sexes <- unique(treatmentPathways$sex)
  sexes <- sexes[sexes != "all"]
  totalSexes <- lapply(sexes, function(sexGroup) {
    treatmentPathways %>%
      dplyr::filter(.data$age == "all", .data$sex == sexGroup, index_year == "all") %>%
      summarize(sum(freq)) %>%
      pull()
  }) %>% unlist() %>% sum()
  
  expect_identical(totalAll, totalSexes)
  
  # Age group
  ages <- unique(treatmentPathways$age)
  ages <- ages[ages != "all"] %>% unlist()
  totalAges <- lapply(ages, function(ageGroup) {
    treatmentPathways %>%
      dplyr::filter(.data$age == ageGroup, .data$sex == "all", index_year == "all") %>%
      summarize(sum(freq)) %>%
      pull()
  }) %>% unlist() %>% sum()
  
  expect_identical(totalAll, totalAges)
  
  # Years
  years <- unique(treatmentPathways$index_year)
  years <- years[years != "all"]
  totalYears <- lapply(years, function(year) {
    treatmentPathways %>%
      dplyr::filter(.data$age == "all", .data$sex == "all", index_year == year) %>%
      summarize(sum(freq)) %>%
      pull()
  }) %>% unlist() %>% sum()
  
  expect_identical(totalAll, totalYears)
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("attrition", {
  skip_if_not(ableToRun()$CDMC)

  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  suppressWarnings(
    TreatmentPatterns::export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      nonePaths = TRUE,
      stratify = TRUE
    )
  )
  
  expect_true(file.exists(file.path(tempDirLocal, "attrition.csv")))
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("stratify, none paths", {
  skip_if_not(ableToRun()$CDMC)

  globals <- suppressWarnings(generateCohortTableCDMC())

  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )

  result <- TreatmentPatterns::export(
    andromeda = andromeda,
    nonePaths = TRUE,
    stratify = TRUE
  )

  tt <- result$treatment_pathways

  result <- TreatmentPatterns::export(
    andromeda = andromeda,
    nonePaths = TRUE,
    stratify = FALSE
  )

  tf <- result$treatment_pathways

  result <- TreatmentPatterns::export(
    andromeda = andromeda,
    nonePaths = FALSE,
    stratify = TRUE
  )

  ft <- result$treatment_pathways

  result <- TreatmentPatterns::export(
    andromeda = andromeda,
    nonePaths = FALSE,
    stratify = FALSE
  )

  ff <- result$treatment_pathways

  # No strata
  expect_true(nrow(ff) + 1 == nrow(tf))
  expect_identical(
    ff,
    tf %>%
      dplyr::filter(.data$pathway != "None")
  )

  # Strata
  expect_identical(
    ft,
    tt %>%
      filter(.data$pathway != "None")
  )

  # Pair-wise comparison
  ages <- unique(tt$age)
  sexes <- unique(tt$sex)
  years <- unique(tt$index_year)

  for (age in ages) {
    for (sex in sexes) {
      for (year in years) {
        n1 <- tt %>%
          dplyr::filter(
            .data$age == age,
            .data$sex == sex,
            .data$index_year == year,
            .data$pathway != "None"
          )

        n2 <- ft %>%
          dplyr::filter(
            .data$age == age,
            .data$sex == sex,
            .data$index_year == year
          )
        expect_identical(n1, n2)
      }
    }
  }

  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})
