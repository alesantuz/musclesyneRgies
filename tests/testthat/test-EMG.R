test_that("EMG-related functions produce correct class and output", {
  data("RAW_DATA")

  data1 <- lapply(RAW_DATA, function(x) subsetEMG(x, cy_max = 3))
  data2 <- lapply(data1, filtEMG)
  data3 <- lapply(data2, function(x) normEMG(x, cycle_div = c(100, 100)))

  expect_s3_class(data1[[1]], "EMG")
  expect_s3_class(data2[[1]], "EMG")
  expect_s3_class(data3[[1]], "data.frame")
})
