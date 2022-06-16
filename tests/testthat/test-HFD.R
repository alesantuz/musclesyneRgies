test_that("HFD works", {
  data(primitive)

  expect_equal(
    musclesyneRgies::HFD(primitive$signal)$Higuchi[[1]],
    expected = 1.05,
    tolerance = 0.02
  )

  expect_error(musclesyneRgies::HFD(primitive, k = "a"))

  primitive$signal[5] <- "A"
  expect_error(musclesyneRgies::HFD(primitive))
})
