test_that("defaults", {
  skip_on_cran()
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summary_event_duration.csv"))

  gg <- plotEventDuration(eventDurations = data)

  expect_s3_class(gg, c("gg", "ggplot"))
  expect_error(plotEventDuration(eventDurations = "data"))
})

test_that("minCellCount", {
  skip_on_cran()
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summary_event_duration.csv"))

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
  skip_on_cran()
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summary_event_duration.csv"))

  gg <- plotEventDuration(
    eventDurations = data,
    treatmentGroups = "both"
  )

  expect_true(length(unique(gg$data$event_name)) > 2)

  gg <- plotEventDuration(
    eventDurations = data,
    treatmentGroups = "individual"
  )

  expect_false(all(c("mono-event", "combination-event") %in% gg$data$event_name))

  gg <- plotEventDuration(
    eventDurations = data,
    treatmentGroups = "group"
  )

  expect_true(all(c("mono-event", "combination-event") %in% gg$data$event_name))
})

test_that("treatment lines", {
  skip_on_cran()
  data <- read.csv(system.file(package = "TreatmentPatterns", "DummyOutput", "summary_event_duration.csv"))

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
