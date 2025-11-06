# Global ----
library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(stringr)

# test_that("computePathways DatabaseConnector", {
#   skip("Eunomia [2.0.0] bug")
#   skip_on_cran()
#   skip_if_not(ableToRun()$CG)
# 
#   expect_message(
#     expect_message(
#       expect_message(
#         computePathways(
#           cohorts = .CG$cohorts,
#           cohortTableName = .CG$cohortTableName,
#           connectionDetails = .CG$connectionDetails,
#           cdmSchema = "main",
#           resultSchema = "main"
#         ),
#         "After maxPathLength: 554"
#       ),
#       "After combinationWindow: 554"
#     ),
#     "Original number of rows: 8366"
#   )
# })

test_that("computePathways CDMConnector", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  expect_message(
    expect_message(
      expect_message(
        computePathways(
          cohorts = .CM$cohorts,
          cdm = .CM$cdm,
          .CM$cohortTableName
        ),
        ">> Starting on"
      ),
      "-- Iteration 1:"
    ),
    "-- treatment construction done"
  )
})

# test_that("nrow exitCohorts > 0", {
#   skip("Eunomia [2.0.0] bug")
#   skip_on_cran()
#   skip_if_not(ableToRun()$CG)
# 
#   cohorts <- .CG$cohorts %>%
#     mutate(type = case_when(
#       .data$cohortName == "Acetaminophen" ~ "exit",
#       .default = .data$type
#     ))
# 
#   expect_message(
#     computePathways(
#       connectionDetails = .CG$connectionDetails,
#       cdmSchema = .CG$cdmSchema,
#       resultSchema = .CG$resultSchema,
#       cohorts = cohorts,
#       cohortTableName = .CG$cohortTableName
#     ),
#     "Records: 2117"
#   )
# })

# Parameter sweep ----
test_that("windowStart", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  expect_error(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      windowStart = "0"
    ),
    "Must be of type.+'integerish'"
  )

  expect_error(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      windowStart = Inf
    ),
    "Must be of type.+'integerish'"
  )

  expect_message(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      windowStart = 0
    ),
    "Records: 8366"
  )

  expect_message(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      windowStart = -30
    ),
    "Records: 8366"
  )

  expect_message(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      windowStart = 30
    ),
    "Records: 6267"
  )
})

test_that("minEraDuration", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  expect_error(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      minEraDuration = "0"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("splitEventCohorts", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  andromeda_empty <- computePathways(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm,
    splitEventCohorts = NULL
  )

  andromeda_Clavulanate <- computePathways(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm,
    splitEventCohorts = 4,
    splitTime = 30
  )

  empty <- andromeda_empty[["treatmentHistory"]] %>% collect()
  clavulanate <- andromeda_Clavulanate[["treatmentHistory"]] %>% collect()

  expect_false(identical(empty$eventCohortId, clavulanate$eventCohortId))

  expect_error(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      splitEventCohorts = "1"
    ),
    "Must be of type.+'integerish'"
  )

  Andromeda::close(andromeda_Clavulanate)
  Andromeda::close(andromeda_empty)
})

test_that("splitTime", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  expect_error(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      splitTime = "1"
    ),
    "Must be of type.+'integerish'"
  )
})

test_that("eraCollapseSize", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  andromeda_0 <- computePathways(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm,
    eraCollapseSize = 0
  )

  andromeda_10000 <- computePathways(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm,
    eraCollapseSize = 10000
  )

  expect_error(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      eraCollapseSize = ""
    ),
    " Must be of type.+'numeric'"
  )

  Andromeda::close(andromeda_0)
  Andromeda::close(andromeda_10000)
})

test_that("combinationWindow", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  expect_error(
    suppressWarnings(
      computePathways(
        cohorts = .CM$cohorts,
        cohortTableName = .CM$cohortTableName,
        cdm = .CM$cdm,
        combinationWindow = ""
      )
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("minPostCombinationDuration: 30", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.Date("2014-01-01"), as.Date("2015-01-01"),
    2,                     5,           as.Date("2014-01-01"), as.Date("2014-03-01"),
    3,                     5,           as.Date("2014-01-10"), as.Date("2014-03-15")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  ## 30 ----
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 3,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  treatmentPaths <- result$treatment_pathways

  pathway <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$index_year == "all"
    ) %>%
    dplyr::pull(.data$pathway)

  expect_identical(pathway, "A+B")

  ## 12 ----
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 3,
    combinationWindow = 30,
    minPostCombinationDuration = 12,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  treatmentPaths <- result$treatment_pathways

  pathway <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$index_year == "all"
    ) %>%
    dplyr::pull(.data$pathway)

  expect_identical(pathway, "A+B-B")


  ## 8 ----
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    minEraDuration = 0,
    eraCollapseSize = 3,
    combinationWindow = 30,
    minPostCombinationDuration = 8,
    filterTreatments = "All",
    maxPathLength = 5
  )

  result <- TreatmentPatterns::export(andromeda, minCellCount = 1)

  treatmentPaths <- result$treatment_pathways

  pathway <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$index_year == "all"
    ) %>%
    dplyr::pull(.data$pathway)

  expect_identical(pathway, "A-A+B-B")
})

test_that("filterTreatments", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  expect_error(
    computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm,
      filterTreatments = ""
    ),
    "Must be a subset of"
  )

  first <- computePathways(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm,
    filterTreatments = "First"
  )

  changes <- computePathways(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm,
    filterTreatments = "Changes"
  )

  all <- computePathways(
    cohorts = .CM$cohorts,
    cohortTableName = .CM$cohortTableName,
    cdm = .CM$cdm,
    filterTreatments = "All"
  )

  firstTH <- first$treatmentHistory %>%
    dplyr::collect()

  changesTH <- changes$treatmentHistory %>%
    dplyr::collect()

  allTH <- all$treatmentHistory %>%
    dplyr::collect()

  # Check names
  expect_identical(
    sort(names(firstTH)),
    sort(names(changesTH)),
    sort(names(allTH))
  )

  # eventCohortId
  expect_identical(
    "character",
    class(firstTH$eventCohortId),
    class(changesTH$eventCohortId),
    class(allTH$eventCohortId)
  )

  expect_contains(
    expected = c(
      class(firstTH$personId),
      class(changesTH$personId),
      class(allTH$personId)
    ),
    object = c("integer", "numeric")
  )
  
  expect_contains(
    expected = c(
      class(firstTH$indexYear),
      class(changesTH$indexYear),
      class(allTH$indexYear)
    ),
    object = c("numeric", "integer")
  )

  expect_contains(
    expected = c(
      class(firstTH$eventStartDate),
      class(changesTH$eventStartDate),
      class(allTH$eventStartDate)
    ),
    object = c("numeric", "integer")
  )

  expect_contains(
    expected = c(
      class(firstTH$eventEndDate),
      class(changesTH$eventStartDate),
      class(allTH$eventEndDate)
    ),
    object = c("numeric", "integer")
  )

  expect_contains(
    expected = c(
      class(firstTH$age),
      class(changesTH$age),
      class(allTH$age)
    ),
    object = c("numeric", "integer")
  )

  expect_identical(
    "character",
    class(firstTH$sex),
    class(changesTH$sex),
    class(allTH$sex)
  )

  expect_contains(
    expected = c(
      class(firstTH$durationEra),
      class(changesTH$durationEra),
      class(allTH$durationEra)
    ),
    object = c("numeric", "integer")
  )

  expect_contains(
    expected = c(
      class(firstTH$sortOrder),
      class(changesTH$sortOrder),
      class(allTH$sortOrder)
    ),
    object = c("numeric", "integer")
  )

  expect_true(
    all(c(class(firstTH$eventSeq), class(changesTH$eventSeq), class(allTH$eventSeq)) %in% c("numeric", "integer"))
  )

  expect_identical(
    "character",
    class(firstTH$eventCohortName),
    class(changesTH$eventCohortName),
    class(allTH$eventCohortName)
  )

  expect_false(any(is.na(firstTH)))
  expect_false(any(is.na(changesTH)))
  expect_false(any(is.na(allTH)))

  expect_false(any(is.null(firstTH)))
  expect_false(any(is.null(changesTH)))
  expect_false(any(is.null(allTH)))

  expect_true(Andromeda::isAndromeda(first))
  expect_true(Andromeda::isAndromeda(changes))
  expect_true(Andromeda::isAndromeda(all))

  Andromeda::close(first)
  Andromeda::close(changes)
  Andromeda::close(all)
})

test_that("FRFS combination", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    1, 5, as.Date("2017-08-03"), as.Date("2018-08-02"),
    3, 5, as.Date("2017-11-26"), as.Date("2018-04-10"),
    2, 5, as.Date("2018-02-26"), as.Date("2018-04-24"),
    1, 7, as.Date("2015-10-20"), as.Date("2018-06-28"),
    2, 7, as.Date("2017-12-04"), as.Date("2018-04-30")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    tempEmulationSchema = NULL,
    minEraDuration = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  nFRFS <- andromeda$addRowsFRFS_1 %>%
    dplyr::collect() %>%
    nrow()

  nLRFS <- andromeda$addRowsLRFS_1 %>%
    dplyr::collect() %>%
    nrow()

  expect_equal(nFRFS, 1)
  expect_equal(nLRFS, 0)
})

test_that("LRFS combination", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomiaDir())

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )

  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    1, 5, as.Date("2017-08-03"), as.Date("2018-08-02"),
    3, 5, as.Date("2017-11-26"), as.Date("2018-04-10"),
    2, 5, as.Date("2018-01-26"), as.Date("2018-03-24"),
    1, 7, as.Date("2015-10-20"), as.Date("2018-06-28"),
    2, 7, as.Date("2017-12-04"), as.Date("2018-04-30")
  )

  copy_to(con, cohort_table, overwrite = TRUE)

  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")

  andromeda <- computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    tempEmulationSchema = NULL,
    minEraDuration = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )

  nFRFS <- andromeda$addRowsFRFS_1 %>%
    dplyr::collect() %>%
    nrow()

  nLRFS <- andromeda$addRowsLRFS_1 %>%
    dplyr::collect() %>%
    nrow()

  expect_equal(nFRFS, 0)
  expect_equal(nLRFS, 1)
})

test_that("No target records", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  .CM$cohorts$cohortId[8] <- 9

  expect_warning({
    outputEnv <- computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm
    )
  })

  expect_true(nrow(outputEnv$treatmentHistory %>% collect()) == 0)
})

test_that("Empty cohort table", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  .CM$cdm$cohort_table <- .CM$cdm$cohort_table %>%
    filter(.data$cohort_definition_id <= 0) %>%
    compute()

  expect_warning({
    outputEnv <- computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm
    )
  })

  expect_true(nrow(outputEnv$treatmentHistory %>% collect()) == 0)
})

test_that("No target defined", {
  skip_on_cran()
  skip_if_not(ableToRun()$CDMC)

  .CM$cohorts$type <- rep("event", 8)

  expect_error({
    outputEnv <- computePathways(
      cohorts = .CM$cohorts,
      cohortTableName = .CM$cohortTableName,
      cdm = .CM$cdm
    )
  })
})

# test_that("Attrition", {
#   skip_on_cran()
#   skip_on_os(os = "linux")
#   skip_if_not(ableToRun()$CDMC)
#   skip_if_not(ableToRun()$CG)
# 
#   outputEnvCDMC <- computePathways(
#     cohorts = .CM$cohorts,
#     cohortTableName = .CM$cohortTableName,
#     cdm = .CM$cdm
#   )
# 
#   outputEnvCG <- computePathways(
#     cohorts = .CM$cohorts,
#     cohortTableName = .CM$cohortTableName,
#     connectionDetails = .CM$connectionDetails,
#     cdmSchema = .CM$cdmSchema,
#     resultSchema = .CM$resultSchema
#   )
# 
#   expect_identical(
#     outputEnvCDMC$attrition %>%
#       collect() %>%
#       select(-"time_stamp"),
#     outputEnvCG$attrition %>%
#       collect() %>%
#       select(-"time_stamp")
#   )
# 
#   Andromeda::close(outputEnvCG)
#   Andromeda::close(outputEnvCDMC)
# })
