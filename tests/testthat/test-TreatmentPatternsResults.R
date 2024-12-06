test_that("Method: new(data.frame)", {
  skip_if_not(ableToRun()$CDMC)
  globals <- suppressWarnings(generateCohortTableCDMC())

  result <- TreatmentPatterns::executeTreatmentPatterns(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )

  expect_s3_class(result$analyses, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$cdm_source_info, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_year, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_sex, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_age, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$summary_event_duration, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$treatment_pathways, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$metadata, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$attrition, class = c("tbl_df", "tbl", "data.frame"))
})

test_that("Method: new(csvDirPath)", {
  result <- TreatmentPatternsResults$new()
  result$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))
  expect_s3_class(result$analyses, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$cdm_source_info, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_year, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_sex, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_age, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$summary_event_duration, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$treatment_pathways, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$metadata, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$attrition, class = c("tbl_df", "tbl", "data.frame"))

  result <- TreatmentPatternsResults$new(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))
  expect_s3_class(result$analyses, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$cdm_source_info, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_year, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_sex, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_age, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$summary_event_duration, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$treatment_pathways, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$metadata, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$attrition, class = c("tbl_df", "tbl", "data.frame"))
})

test_that("Method: new(zipFile)", {
  result <- TreatmentPatternsResults$new()
  result$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip"))
  expect_s3_class(result$analyses, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$cdm_source_info, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_year, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_sex, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_age, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$summary_event_duration, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$treatment_pathways, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$metadata, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$attrition, class = c("tbl_df", "tbl", "data.frame"))

  result <- TreatmentPatternsResults$new(filePath = system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip"))
  expect_s3_class(result$analyses, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$cdm_source_info, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_year, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_sex, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$counts_age, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$summary_event_duration, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$treatment_pathways, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$metadata, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(result$attrition, class = c("tbl_df", "tbl", "data.frame"))
})

test_that("Method: new(wrongFile)", {
  expect_error(TreatmentPatternsResults$new(filePath = "some/file.xyz"))
})

test_that("Method: plotEventDuration()", {
  results <- TreatmentPatternsResults$new()
  results$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))

  gg <- results$plotEventDuration()

  expect_s3_class(gg, class = c("gg", "ggplot"))
})

test_that("Method: plotSankey()", {
  results <- TreatmentPatternsResults$new()
  results$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))
  
  sankey <- results$plotSankey(nonePaths = TRUE)
  expect_s3_class(sankey, class = c("sankeyNetwork", "htmlwidget"))

  sankey <- results$plotSankey(nonePaths = FALSE)
  expect_s3_class(sankey, class = c("sankeyNetwork", "htmlwidget"))
})

test_that("Method: plotSunburst()", {
  results <- TreatmentPatternsResults$new()
  results$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))
  
  sunburst <- results$plotSunburst(nonePaths = TRUE)
  expect_s3_class(sunburst, class = c("sunburst", "htmlwidget"))

  sunburst <- results$plotSunburst(nonePaths = FALSE)
  expect_s3_class(sunburst, class = c("sunburst", "htmlwidget"))
})

test_that("Method: saveAsCsv()", {
  results <- TreatmentPatternsResults$new()
  results$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))
  
  tempDir <- file.path(tempdir(), "saveAsCsv")
  
  expect_message(
    results$saveAsCsv(path = tempDir)
  )
  
  expect_true(all(
    list.files(tempDir) %in% c(
      "analyses.csv", "attrition.csv", "cdm_source_info.csv", "counts_age.csv",
      "counts_sex.csv", "counts_year.csv", "metadata.csv",
      "summary_event_duration.csv", "treatment_pathways.csv"
    )
  ))

  unlink(tempDir, recursive = TRUE)
})

test_that("Method: saveAsZip()", {
  results <- TreatmentPatternsResults$new()
  results$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))
  
  tempDir <- file.path(tempdir(), "saveAsZip")
  
  expect_message(
    results$saveAsZip(path = tempDir, name = "output.zip")
  )
  
  expect_true(all(
    list.files(tempDir) %in% "output.zip"
  ))

  unlink(tempDir, recursive = TRUE)
})

test_that("Method: uploadResultsToDb()", {
  skip_if_not(require("DatabaseConnector", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE))
  skip_if_not(require("ResultModelManager", quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE))

  results <- TreatmentPatternsResults$new()
  results$load(filePath = system.file(package = "TreatmentPatterns", "DummyOutput"))

  tempDir <- file.path(tempdir(), "test-uploadToDb")
  dir.create(tempDir, showWarnings = FALSE, recursive = TRUE)

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = file.path(tempDir, "db.sqlite")
  )

  suppressWarnings(
    results$uploadResultsToDb(
    connectionDetails = connectionDetails,
    schema = "main",
    prefix = "tp_",
    overwrite = TRUE
    )
  )

  con <- DatabaseConnector::connect(connectionDetails)

  tableNames <- DatabaseConnector::renderTranslateQuerySql(con, "SELECT name FROM sqlite_master WHERE type='table';")
  tableNames <- substr(tableNames$NAME, start = 4, 100)

  for (name in tableNames) {
    expect_s3_class(results[[name]], class = "data.frame")
  }

  DatabaseConnector::disconnect(con)
  unlink(tempDir, recursive = TRUE)
})
