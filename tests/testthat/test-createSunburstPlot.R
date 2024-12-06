library(testthat)
library(TreatmentPatterns)

dummyData <- data.frame(
  pathway = c("A+Z", "B", "C", "A-B", "B-C", "D+Z", "E", "F", "D-E", "D-E-F"),
  freq = c(25, 25, 25, 12, 13, 25, 25, 25, 12, 13),
  sex = rep("all", 10),
  age = rep("all", 10),
  index_year = c(rep("all", 5), rep("2020", 5))
)

test_that("minimal", {
  p <- createSunburstPlot(treatmentPathways = dummyData)
  expect_s3_class(p$x$data, "json")
})

test_that("groupCombinations: TRUE", {
  p <- createSunburstPlot(
    treatmentPathways = dummyData,
    groupCombinations = TRUE
  )
  
  expect_s3_class(p$x$data, "json")
})

test_that("colors", {
  actualColors <- c("#ff33cc", "#ff0000", "#00ff00", "#0000ff", "#ffffff", "#000000")
  
  p <- createSunburstPlot(treatmentPathways = dummyData, colors = actualColors)
  
  pColors <- p$x$options$colors
  
  expect_identical(pColors, actualColors)
})
