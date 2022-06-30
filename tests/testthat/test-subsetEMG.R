test_that("subsetEMG works", {
  data("RAW_DATA")
  data("SYNS")

  # Expect correct number of cycles after subset
  test <- musclesyneRgies::subsetEMG(
    x = RAW_DATA[[1]],
    cy_max = 3,
    cy_start = 2
  )
  expect_equal(nrow(test$cycles), 4)

  test <- musclesyneRgies::subsetEMG(
    x = RAW_DATA[[1]],
    cy_max = 30,
    cy_start = 5
  )
  expect_equal(nrow(test$cycles), 2)

  # Expect error if input is not in the correct format
  expect_error(musclesyneRgies::subsetEMG(SYNS[[1]]))
})
