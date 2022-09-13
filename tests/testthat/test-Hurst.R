test_that("Hurst works", {
  data(act_pattern)

  expect_equal(
    musclesyneRgies::Hurst(act_pattern$signal)$Hurst[[1]],
    expected = 0.72,
    tolerance = 0.02
  )

  expect_error(musclesyneRgies::Hurst(act_pattern$signal, k = "a"))

  act_pattern$signal[5] <- "A"
  expect_error(musclesyneRgies::Hurst(act_pattern$signal))
})
