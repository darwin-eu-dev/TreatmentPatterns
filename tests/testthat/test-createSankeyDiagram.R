library(TreatmentPatterns)
library(testthat)

dummyData <- data.frame(
  pathway = c("A+Z", "B", "C", "A-B", "B-C", "D+Z", "E", "F", "D-E", "D-E-F"),
  freq = c(25, 25, 25, 12, 13, 25, 25, 25, 12, 13),
  sex = rep("all", 10),
  age = rep("all", 10),
  index_year = c(rep("all", 5), rep("2020", 5))
)

test_that("void", {
  skip_on_cran()
  expect_error(createSankeyDiagram())
})

test_that("minimal", {
  skip_on_cran()
  p <- createSankeyDiagram(treatmentPathways = dummyData)

  pLabels <- stringr::str_remove_all(string = p$x$nodes$name, pattern = "\\d\\.")
  pLabels <- pLabels["Stopped" != pLabels] |>
    unique() |>
    sort()

  actualLabels <- stringr::str_split(string = dummyData$path, pattern = "-") |>
    unlist() |>
    unique() |>
    sort()

  expect_identical(pLabels, actualLabels)
})

test_that("groupCombinations: TRUE", {
  skip_on_cran()
  p <- createSankeyDiagram(treatmentPathways = dummyData, groupCombinations = TRUE)

  pLabels <- stringr::str_remove_all(string = p$x$nodes$name, pattern = "\\d\\.")
  pLabels <- pLabels["Stopped" != pLabels] |>
    unique() |>
    sort()

  actualLabels <- stringr::str_split(string = dummyData$path, pattern = "-") |>
    unlist() |>
    stringr::str_replace_all(pattern = ".+\\+.+", replacement = "Combination") |>
    unlist() |>
    unique() |>
    sort()

  expect_identical(pLabels, actualLabels)

  df <- data.frame(
    pathway = c(
      "A-B",
      "A+B",
      "A-B+C",
      "A+B-C",
      "A+B+C",
      "A-B+C-D",
      "Z_Y-A",
      "Z_Y+A",
      "1_2-A",
      "1_2+A"
    ),
    freq = rep(100, 10)
  )

  p <- createSankeyDiagram(treatmentPathways = df, groupCombinations = TRUE)
  labels <- stringr::str_remove_all(string = p$x$nodes$name, pattern = "\\d\\.")

  expect_true(all(labels %in% c("A", "B", "C", "D", "Z_Y", "1_2", "Combination", "Stopped")))

  expect_identical(
    p$x$nodes$name,
    c("1.1_2", "1.A", "1.Combination", "1.Z_Y", "2.A", "2.B", "2.Combination", "2.C", "2.Stopped", "3.Stopped", "3.D")
  )
})

test_that("colors", {
  skip_on_cran()
  actualColors <- c("#ff33cc", "#ff0000", "#00ff00", "#0000ff", "#ffffff", "#000000")

  p <- createSankeyDiagram(treatmentPathways = dummyData, colors = actualColors)

  pColors <- unlist(stringr::str_extract_all(string = p$x$options$colourScale, "\\#\\w{6}"))

  expect_true(all(pColors %in% actualColors))

  actualColors <- list(
    A = "#0FFFF0",
    B = "#F0F0F0",
    C = "#FF0000",
    D = "#FFF000",
    E = "#00FF00",
    `F` = "#F000FF",
    `A+Z` = "#0000FF",
    `D+Z` = "#0FF0F0"
  )

  p <- createSankeyDiagram(treatmentPathways = dummyData, colors = actualColors)

  pColors <- unlist(stringr::str_extract_all(string = p$x$options$colourScale, "\\#\\w{6}"))
  labels <- unlist(stringr::str_extract_all(string = p$x$options$colourScale, "\\'\\d\\.[\\w\\+\\-]+\\'"))
  labels <- stringr::str_remove_all(string = labels, pattern = "[\\'|\\d{1}\\.]")
  labels <- unique(labels)

  l <- as.list(unique(pColors))
  names(l) <- labels
  l <- l[names(l) != "Stopped"]

  expect_identical(
    actualColors[order(names(actualColors))],
    l[order(names(l))]
  )
})

test_that("2 path levels", {
  skip_on_cran()
  dummyData <- data.frame(
    pathway = c("A", "A-B+C", "A-D", "B+C", "D"),
    freq = c(206, 6, 14, 48, 221),
    sex = rep("all", 5),
    age = rep("all", 5),
    index_year = rep("all", 5)
  )

  p <- createSankeyDiagram(treatmentPathways = dummyData)

  pLabels <- stringr::str_remove_all(string = p$x$nodes$name, pattern = "\\d\\.")
  pLabels <- pLabels["Stopped" != pLabels] |>
    unique() |>
    sort()

  actualLabels <- stringr::str_split(string = dummyData$path, pattern = "-") |>
    unlist() |>
    unique() |>
    sort()

  expect_identical(pLabels, actualLabels)
})

test_that("1 path levels", {
  skip_on_cran()
  treatmentPathways <- data.frame(
    pathway = c("a", "b", "c"),
    freq = c(55, 8, 11),
    sex = rep("all", 3),
    age = rep("all", 3),
    index_year = rep("all", 3)
  )

  expect_error(
    createSankeyDiagram(treatmentPathways),
    "Cannot compute Sankey Diagram as there is only one level in the data."
  )
})
