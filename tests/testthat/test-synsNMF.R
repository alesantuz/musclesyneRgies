test_that("NMF produces correct class and output", {
  data(FILT_EMG)
  SYNS <- lapply(FILT_EMG, synsNMF)
  SYNS_fixed <- lapply(FILT_EMG, synsNMF, fixed_syns = 2)

  # Class
  expect_s3_class(SYNS$ID0012_TW_01, "musclesyneRgies")

  # Rank
  expect_equal(SYNS$ID0012_TW_01$syns, 4)
  expect_equal(SYNS_fixed$ID0012_TW_01$syns, 2)

  # Number of muscles
  expect_equal(nrow(SYNS$ID0012_TW_01$M), 13)
  expect_equal(nrow(SYNS_fixed$ID0012_TW_01$M), 13)

  # Classification
  expect_equal(SYNS$ID0012_TW_01$classification, "none")
  expect_equal(SYNS_fixed$ID0012_TW_01$classification, "none")

  # Rank type
  expect_equal(SYNS$ID0012_TW_01$rank_type, "variable")
  expect_equal(SYNS_fixed$ID0012_TW_01$rank_type, "fixed")
})
