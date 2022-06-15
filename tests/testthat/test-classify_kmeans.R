test_that("k-means classification is robust", {
  data("SYNS")
  SYNS_classified <- classify_kmeans(SYNS)

  for (tt in 1:5) {
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

})
