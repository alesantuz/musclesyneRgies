testthat::test_that("Legacy arguments are deprecated", {
  data("SYNS")

  set.seed(123)
  classified <- classify_kmeans(
    SYNS, clusters = 4, show_plot = FALSE
  )

  lifecycle::expect_deprecated(
    invisible(plot_classified_syns(
      classified,
      ylim_min_M = 0,
      show_plot = FALSE
    )),
    "ylim_min_M"
  )

  lifecycle::expect_deprecated(
    invisible(plot_classified_syns(
      classified,
      ylim_max_M = 1,
      show_plot = FALSE
    )),
    "ylim_max_M"
  )

  testthat::expect_error(
    plot_classified_syns(
      classified,
      ylim_min_W = 0,
      ylim_min_M = 0,
      show_plot = FALSE
    ),
    "Specify only one"
  )
})
