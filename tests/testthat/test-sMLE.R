test_that("sMLE works", {
  data(primitives)

  expect_s3_class(primitives, class = "musclesyneRgies")

  expect_equal(
    musclesyneRgies::sMLE(primitives,
      mean_period = 100,
      future_pts = 300,
      norm = "z",
      pts = 50
    )$sMLE,
    expected = 0.26,
    tolerance = 0.02
  )
})
