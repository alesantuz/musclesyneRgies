test_that("FWHM returns correct values", {
  time_series <- c(
    rep(10, 3),
    rep(11, 3),
    rep(10, 3)
  )

  expect_equal(musclesyneRgies::FWHM(time_series), 3)
  expect_equal(musclesyneRgies::FWHM(time_series, sub_minimum = FALSE), length(time_series))
})
