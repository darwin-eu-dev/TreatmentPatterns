test_that("getResultsDataModelSpecifications", {
  skip_on_cran()
  expect_s3_class(getResultsDataModelSpecifications(), "data.frame")
})
