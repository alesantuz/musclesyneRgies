testthat::test_that("NMF returns canonical and legacy weights", {
  data("FILT_EMG")

  x <- synsNMF(
    FILT_EMG[[1]],
    fixed_syns = 4,
    runs = 1
  )

  testthat::expect_true("W" %in% names(x))
  testthat::expect_true("M" %in% names(x))
  testthat::expect_identical(x$W, x$M)

  testthat::expect_equal(
    unname(x$W %*% t(as.matrix(x$P[, -1]))),
    unname(x$Vr),
    tolerance = 1e-8
  )
})

testthat::test_that("Old and new weight fields are readable", {
  x <- list(M = matrix(1:6, 3, 2))
  testthat::expect_identical(
    musclesyneRgies:::.get_W(x),
    x$M
  )

  x$W <- matrix(7:12, 3, 2)
  testthat::expect_identical(
    musclesyneRgies:::.get_W(x),
    x$W
  )
})

testthat::test_that("Weight aliases remain synchronized", {
  x <- list(M = matrix(1:6, 3, 2))
  W <- matrix(7:12, 3, 2)

  y <- musclesyneRgies:::.set_W(x, W)

  testthat::expect_identical(y$W, W)
  testthat::expect_identical(y$M, W)
})
