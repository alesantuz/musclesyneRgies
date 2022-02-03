test_that("CoA returns correct values", {
  time_series1 <- c(
    0, 1, 2, 3, 3, 2, 1, 0
  )

  time_series2 <- c(
    rep(0, 10),
    time_series1,
    rep(0, 10),
    time_series1,
    rep(0, 10)
  )

  expect_equal(musclesyneRgies::CoA(time_series1), length(time_series1)/2)
  expect_equal(musclesyneRgies::CoA(time_series2), length(time_series2)/2)
})
