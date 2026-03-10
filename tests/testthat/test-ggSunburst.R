test_that("ggSunburst", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

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
    dplyr::group_by(NULL) |>
    mutate(
      frac = .data$freq / total * 100
    )

  # "A" layer 1
  testthat::expect_identical(
    treatmentPathways |>
      dplyr::filter(startsWith(.data$pathway, prefix = "A")) |>
      dplyr::reframe(freq = sum(freq)) |>
      dplyr::pull(.data$freq),

    gg$data |>
      dplyr::filter(.data$event_org == "A", .data$layer == 1) |>
      dplyr::pull("freq")
  )

  # "B" layer 1
  testthat::expect_identical(
    treatmentPathways |>
      dplyr::filter(.data$pathway %in% c("B", "B-A-C")) |>
      dplyr::reframe(freq = sum(freq)) |>
      dplyr::pull(.data$freq),
    
    gg$data |>
      dplyr::filter(.data$event_org == "B", .data$layer == 1) |>
      dplyr::pull("freq")
  )

  # "B" layer 2
  testthat::expect_identical(
    treatmentPathways |>
      dplyr::filter(.data$pathway %in% c("C-B", "A-B-C")) |>
      dplyr::reframe(freq = sum(freq)) |>
      dplyr::pull(.data$freq),

    gg$data |>
      dplyr::filter(.data$event_org == "B", .data$layer == 2) |>
      dplyr::pull("freq") |>
      sum()
  )


  expect_true(any(class(gg) %in% c("gg", "ggplot")))
})
