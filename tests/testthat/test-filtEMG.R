test_that("Input is of the right class", {
  data(RAW_DATA)
  class(RAW_DATA[[1]]) <- "list"
  expect_error(filtEMG(RAW_DATA))
})
