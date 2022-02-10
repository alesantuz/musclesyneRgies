test_that("Plot function return gtable objects", {
  data("RAW_DATA")
  data("FILT_EMG")
  data("SYNS")
  SYNS_classified <- classify_kmeans(SYNS)

  p1 <- plot_rawEMG(
    RAW_DATA[[1]],
    trial = names(RAW_DATA)[1],
    show_plot = FALSE
  )
  p2 <- plot_meanEMG(
    FILT_EMG[[1]],
    trial = names(FILT_EMG)[1],
    show_plot = FALSE
  )
  p3 <- plot_syn_trials(
    SYNS[[1]],
    max_syns = max(unlist(lapply(SYNS, function(x) x$syns))),
    trial = names(SYNS)[1],
    show_plot = FALSE
  )
  p4 <- plot_classified_syns(
    SYNS_classified,
    condition = "TW",
    show_plot = FALSE
  )
  p5 <- plot_classified_syns_UMAP(
    SYNS_classified,
    condition = "TW",
    show_plot = FALSE
  )

  expect_s3_class(p1, "gtable")
  expect_s3_class(p2, "gtable")
  expect_s3_class(p3, "gtable")
  expect_s3_class(p4, "gtable")
  expect_s3_class(p5, "gtable")
})
