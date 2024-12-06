test_that("getResultsDataModelSpecifications", {
  expect_s3_class(getResultsDataModelSpecifications(), "data.frame")
})
