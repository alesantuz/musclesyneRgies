test_that("k-means classification is robust", {
  data("SYNS")

  for (tt in 1:5) {
    SYNS_classified <- musclesyneRgies::classify_kmeans(SYNS)
    ranks <- unique(
      unlist(
        lapply(
          SYNS_classified, function(x) {
            syns <- colnames(x$M)
            if (any(grepl("Syncombined", syns))) syns <- syns[-grep("Syncombined", syns)]
            return(max(as.numeric(gsub("Syn", "", syns))))
          }
        )
      )
    )
    expect_length(unique(ranks), 1)
    expect_equal(unique(ranks), 4)
  }

  # Check that the number of clusters can be imposed
  clust_test <- musclesyneRgies::classify_kmeans(
    SYNS,
    clusters = 2
  ) |>
    lapply(
      function(x) {
        synergies <- colnames(x$M)[!grepl("Syncombined", colnames(x$M))] |>
          gsub("Syn", "", x = _) |>
          as.numeric() |>
          max(na.rm = TRUE)
      }
    ) |>
    unlist() |>
    unique()

  expect_equal(clust_test, 2)

  unique(
    unlist(
      lapply(
        musclesyneRgies::classify_kmeans(
          SYNS,
          clusters = 2
        ),
        function(x) {

        }
      )
    )
  )

  # Check for same amount of normalisation points
  rnd_trial <- sample(seq_len(length(SYNS)), size = 1)
  SYNS_err <- SYNS
  SYNS_err[[rnd_trial]]$P <- SYNS_err[[rnd_trial]]$P[-which(SYNS_err[[rnd_trial]]$P$time > 150), ]
  expect_error(musclesyneRgies::classify_kmeans(SYNS_err))

  # Check for same amount of muscles
  rnd_trial <- sample(seq_len(length(SYNS)), size = 1)
  SYNS_err <- SYNS
  SYNS_err[[rnd_trial]]$P$MA <- NULL
  SYNS_err[[rnd_trial]]$M <- SYNS_err[[rnd_trial]]$M[-grep("MA", rownames(SYNS_err[[rnd_trial]]$M)), ]
  expect_error(musclesyneRgies::classify_kmeans(SYNS_err))

})
