#' Plot 2D UMAP of muscle synergies
#'
#' @param x List of objects of class `musclesyneRgies` (must be classified)
#' @param condition Character: the condition that is being analysed, for archiving purposes
#' @param show_plot Logical, to decide whether plots should be plotted in the active graphic device
#'
#' @details
#' If `show_plot` is TRUE (default) plots are also shown in the active graphic device.
#' Plots can then be saved with the preferred export method, such as `ggplot2::ggsave`.
#'
#' @return 2D UMAP plot of classified synergies.
#'
#' @export
#'
#' @examples
#' # Load some data
#' data(SYNS)
#'
#' # Classify synergies with k-means
#' SYNS_classified <- classify_kmeans(SYNS)
#'
#' # Save plot
#' pp <- plot_classified_syns_UMAP(SYNS_classified,
#'   condition = "TW",
#'   show_plot = FALSE
#' )
plot_classified_syns_UMAP <- function(x,
                                      condition,
                                      show_plot = TRUE) {
  if (interactive()) message("\nCreating 2D UMAP synergy plots for condition ", condition, "...")

  UMAP1 <- UMAP2 <- syn <- NULL

  # Get classification method
  class_method <- unique(unlist(lapply(x, function(y) y$classification)))

  if (class_method == "none" || length(class_method) > 1) {
    stop("Synergies must be classified")
  } else {
    # Get motor modules and concatenated motor primitives
    SYNS_M <- lapply(x, function(y) y$M)
    SYNS_P <- lapply(x, function(y) y$P)
  }

  # Find maximum number of synergies, combined excluded
  max_syns <- max(unlist(lapply(SYNS_M, function(y) {
    y <- data.frame(y)
    y[, grep("combined", colnames(y))] <- NULL
    return(ncol(y))
  })))

  # Calculate mean motor primitives
  SYNS_P <- lapply(SYNS_P, function(y) {
    points <- max(y$time)
    y$time <- NULL

    if (ncol(y) > 0) {
      temp <- matrix(0, nrow = points, ncol = ncol(y))

      colnames(temp) <- colnames(y)

      for (cc in seq(1, (1 + nrow(y) - points), points)) {
        temp <- temp + y[c(cc:(cc + points - 1)), ]
      }

      # Divide by the number of cycles to get mean value
      temp <- temp / (nrow(y) / points)

      # Minimum subtraction
      x <- apply(temp, 2, function(z) z - min(z))
      # Amplitude normalisation
      x <- apply(temp, 2, function(z) z / max(z))
    }

    return(data.frame(x))
  })

  points <- unique(unlist(lapply(SYNS_P, function(y) nrow(y))))
  muscles <- unique(unlist(lapply(SYNS_M, function(y) rownames(y))))

  # UMAP of synergies
  data_P <- lapply(SYNS_P, function(y) data.frame(synergy = colnames(y), t(y)))
  data_M <- lapply(SYNS_M, function(y) t(y))
  data_P <- plyr::ldply(data_P, data.frame, .id = "synergy")
  data_M <- plyr::ldply(data_M, data.frame, .id = "synergy")

  data_syns <- data_P$synergy
  if (any(grepl("Syncombined\\.", data_syns))) {
    data_syns <- gsub("Syncombined.+", "Syncombined", data_syns)
  }

  data_P$synergy <- NULL
  data_M$synergy <- NULL

  umap_P <- data.frame(data_syns, umap::umap(data_P)$layout)
  umap_M <- data.frame(data_syns, umap::umap(data_M)$layout)

  colnames(umap_P) <- c("syn", "UMAP1", "UMAP2")
  colnames(umap_M) <- colnames(umap_P)

  ggumap_M <- ggplot2::ggplot(
    data = umap_M,
    ggplot2::aes(
      x = UMAP1, y = UMAP2,
      colour = factor(syn)
    )
  ) +
    ggplot2::geom_point(size = 4, alpha = 0.5) +
    ggplot2::ggtitle(paste0(condition, " - UMAP (motor modules)")) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  ggumap_P <- ggplot2::ggplot(
    data = umap_P,
    ggplot2::aes(
      x = UMAP1, y = UMAP2,
      colour = factor(syn)
    )
  ) +
    ggplot2::geom_point(size = 4, alpha = 0.5) +
    ggplot2::ggtitle(paste0(condition, " - UMAP (motor primitives)")) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  # Arrange plots nicely and return as gtable
  gg <- gridExtra::arrangeGrob(
    grobs = list(ggumap_M, ggumap_P),
    nrow = 2,
    ncol = 1
  )
  # Plot on active graphic device if needed
  if (show_plot) {
    # Prepare graphic device
    graphics::plot.new()
    # Arrange
    gridExtra::grid.arrange(gg, newpage = FALSE)
  }
  return(gg)
  if (interactive()) message("...done!")
}
