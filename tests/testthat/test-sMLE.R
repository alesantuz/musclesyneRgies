test_that("sMLE works", {
  data(act_patterns)

  expect_equal(
    musclesyneRgies::sMLE(act_patterns,
      mean_period = 100,
      future_pts = 300,
      norm = "z",
      pts = 50
    )$sMLE,
    expected = 0.26,
    tolerance = 0.02
  )

  class(act_patterns) <- "badclass"
  expect_error(musclesyneRgies::sMLE(act_patterns))

})
