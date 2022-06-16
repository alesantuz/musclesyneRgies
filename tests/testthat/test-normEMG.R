test_that("normEMG works", {
  data("RAW_DATA")
  # Filter raw EMG
  filtered_EMG <- lapply(RAW_DATA, function(x) {
    musclesyneRgies::filtEMG(x, HPf = 50, HPo = 4, LPf = 20, LPo = 4)
  })

  expect_s3_class(filtered_EMG[[1]], class = "EMG")

  expect_error(
    musclesyneRgies::normEMG(
      filtered_EMG[[1]],
      cy_max = 3,
      cycle_div = c(100, 100, 100)
    )
  )

  class(filtered_EMG[[1]]) <- "badclass"

  expect_error(
    musclesyneRgies::normEMG(
      filtered_EMG[[1]],
      cycle_div = c(100, 100)
    )
  )
})
