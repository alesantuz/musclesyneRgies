test_that("CoA returns correct values", {
  time_series1 <- c(
    1, 2, 3, 4, 4, 3, 2, 1
  )

  time_series2 <- c(
    rep(1, 10),
    time_series1,
    rep(1, 10),
    time_series1,
    rep(1, 10)
  )

  expect_equal(musclesyneRgies::CoA(time_series1), length(time_series1)/2)
  expect_equal(musclesyneRgies::CoA(time_series2), length(time_series2)/2)
})
