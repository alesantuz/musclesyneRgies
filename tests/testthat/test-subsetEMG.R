test_that("subsetEMG works", {
  data("SYNS")
  expect_error(musclesyneRgies::subsetEMG(SYNS[[1]]))
})
