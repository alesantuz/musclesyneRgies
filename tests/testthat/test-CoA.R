test_that("CoA returns correct values (edge-based, 1-based)", {
  time_series1 <- c(1, 2, 3, 4, 4, 3, 2, 1)  # n = 8 -> 4.5
  time_series2 <- c(
    rep(1, 10), time_series1, rep(1, 10), time_series1, rep(1, 10)
  ) # n = 46 -> 23.5

  expect_equal(musclesyneRgies::CoA(time_series1),
               (length(time_series1) + 1) / 2, tolerance = 1e-12)
  expect_equal(musclesyneRgies::CoA(time_series2),
               (length(time_series2) + 1) / 2, tolerance = 1e-12)
})

test_that("CoA is undefined for near-uniform signals", {
  set.seed(1)
  x <- rep(1, 200) + rnorm(200, sd = 1e-12)
  expect_true(is.na(CoA(x, tol_rel = 1e-6)))
})

test_that("CoA is undefined for uniform signal", {
  x <- rep(1, 200)
  expect_true(is.na(CoA(x)))
})
