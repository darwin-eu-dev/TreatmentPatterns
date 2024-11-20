test_that("defaults", {
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summaryEventDuration.csv"))

  gg <- plotEventDuration(eventDurations = data)
  
  expect_class(x = gg, classes = c("gg", "ggplot"))
  expect_error(plotEventDuration(eventDurations = "data"))
})

test_that("minCellCount", {
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summaryEventDuration.csv"))

  gg <- plotEventDuration(
    eventDurations = data,
    minCellCount = 30
  )

  expect_true(all(gg$data$count >= 30))
  expect_false(any(gg$data$count < 30))
  expect_error(
    plotEventDuration(
      eventDurations = data,
      minCellCount = Inf
    )
  )
  expect_error(
    plotEventDuration(
      eventDurations = data,
      minCellCount = -30
    )
  )
})

test_that("treatmentGroups", {
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summaryEventDuration.csv"))
  
  gg <- plotEventDuration(
    eventDurations = data,
    treatmentGroups = "both"
  )
  
  expect_true(length(unique(gg$data$eventName)) > 2)

  gg <- plotEventDuration(
    eventDurations = data,
    treatmentGroups = "individual"
  )

  expect_false(all(c("mono-event", "combination-event") %in% gg$data$eventName))
  
  gg <- plotEventDuration(
    eventDurations = data,
    treatmentGroups = "group"
  )
  
  expect_true(all(c("mono-event", "combination-event") %in% gg$data$eventName))
})

test_that("treatment lines", {
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summaryEventDuration.csv"))

  gg <- plotEventDuration(
    eventDurations = data,
    eventLines = NULL
  )

  expect_true(length(unique(gg$data$line)) == 6)

  gg <- plotEventDuration(
    eventDurations = data,
    eventLines = c(1, 2, 3)
  )

  expect_true(length(unique(gg$data$line)) == 4)

  expect_error(
    plotEventDuration(
      eventDurations = data,
      eventLines = "3"
    )
  )

  gg <- plotEventDuration(
    eventDurations = data,
    eventLines = c(1, 2, 3),
    includeOverall = FALSE
  )
  
  expect_true(length(unique(gg$data$line)) == 3)
})
