test_that("ggSunburst", {
  treatmentPathways <- data.frame(
    pathway = c("A", "C-B", "A-B-C", "B", "B+A", "B-A-C"),
    freq = c(100, 75, 25, 500, 350, 20),
    age = "all",
    sex = "all",
    index_year = "all"
  )

  gg <- ggSunburst(treatmentPathways)
  
  total <- sum(treatmentPathways$freq)
  df <- treatmentPathways %>%
    mutate(
      frac = .data$freq / total * 100
    )
  
  expect_true(all(unique(gg$data$frac) %in% unique(df$frac)))
  expect_true(all(class(gg) %in% c("gg", "ggplot")))
})

test_that("ggSunburst: groupCombinations", {
  treatmentPathways <- data.frame(
    pathway = c("A", "C-B", "A-B-C", "B", "B+A", "B-A-C"),
    freq = c(100, 75, 25, 500, 350, 20),
    age = "all",
    sex = "all",
    index_year = "all"
  )
  
  gg <- ggSunburst(treatmentPathways, groupCombinations = TRUE)
  
  expect_true("Combination" %in% gg$data$label)
  expect_false(all(grepl("\\+", gg$data$label)))
})

test_that("ggSunburst: unit", {
  treatmentPathways <- data.frame(
    pathway = c("A", "C-B", "A-B-C", "B", "B+A", "B-A-C"),
    freq = c(100, 75, 25, 500, 350, 20),
    age = "all",
    sex = "all",
    index_year = "all"
  )
  
  gg <- ggSunburst(treatmentPathways, unit = "count")
  
  expect_true(all(unique(gg$data$frac) %in% unique(treatmentPathways$freq)))
})
