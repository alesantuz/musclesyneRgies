test_that("EMG-related functions produce correct class and output
          and independent on number of columns in cycles", {
  data("RAW_DATA")

  data1 <- lapply(RAW_DATA, function(x) musclesyneRgies::subsetEMG(x, cy_max = 3))
  data2 <- lapply(data1, filtEMG)
  data3 <- lapply(data2, function(x) musclesyneRgies::normEMG(x, cycle_div = c(100, 100)))
  data4 <- lapply(RAW_DATA, function(x) {
    x$cycles <- x$cycles$V1
    return(x)
  }) |>
    lapply(function(x) musclesyneRgies::subsetEMG(x, cy_max = 3)) |>
    lapply(filtEMG) |>
    lapply(function(x) musclesyneRgies::normEMG(x, cycle_div = 100))

  expect_s3_class(data1[[1]], "EMG")
  expect_s3_class(data2[[1]], "EMG")
  expect_s3_class(data3[[1]], "data.frame")
  expect_s3_class(data4[[1]], "data.frame")
})
