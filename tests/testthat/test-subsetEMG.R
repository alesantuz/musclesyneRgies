test_that("subsetEMG works", {
  data("RAW_DATA")
  expect_s3_class(RAW_DATA[[1]], "EMG")
})
