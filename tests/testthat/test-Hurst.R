test_that("Hurst works", {
  data(primitive)

  expect_equal(
    musclesyneRgies::Hurst(primitive$signal)$Hurst[[1]],
    expected = 0.72,
    tolerance = 0.02
  )
})
