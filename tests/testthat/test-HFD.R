test_that("HFD works", {
  data(act_pattern)

  expect_equal(
    musclesyneRgies::HFD(act_pattern$signal)$Higuchi[[1]],
    expected = 1.05,
    tolerance = 0.02
  )

  expect_error(musclesyneRgies::HFD(act_pattern, k = "a"))

  act_pattern$signal[5] <- "A"
  expect_error(musclesyneRgies::HFD(act_pattern))
})
