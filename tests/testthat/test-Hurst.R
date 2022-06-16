test_that("Hurst works", {
  data(primitive)

  expect_equal(
    musclesyneRgies::Hurst(primitive$signal)$Hurst[[1]],
    expected = 0.72,
    tolerance = 0.02
  )

  expect_error(musclesyneRgies::Hurst(primitive$signal, k = "a"))

  primitive$signal[5] <- "A"
  expect_error(musclesyneRgies::Hurst(primitive$signal))
})
