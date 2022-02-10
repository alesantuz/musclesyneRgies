#' Plot muscle synergies
#'
#' @param x List of objects of class `musclesyneRgies` (must be classified)
#' @param dark_mode To enable dark mode
#' @param line_size Line thickness
#' @param dot_size Dot size on motor modules
#' @param line_col Line colour
#' @param sd_col Standard deviation ribbon colour
#' @param condition Character: the condition that is being analysed, for archiving purposes
#' @param show_plot Logical, to decide whether plots should be plotted in the active graphic device
#'
#' @details
#' If `show_plot` is TRUE (default) plots are also shown in the active graphic device.
#' Plots can then be saved with the preferred export method, such as `ggplot2::ggsave`.
#'
#' @return Global plot containing the average classified muscle synergies and individual trials
#' (motor modules) or standard deviations (motor primitives)
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
#' # Save plot of classified synergies
#' pp <- plot_classified_syns(SYNS_classified,
#'   dark_mode = TRUE,
#'   line_col = "tomato1",
#'   sd_col = "tomato4",
#'   condition = "TW",
#'   show_plot = FALSE
#' )
plot_classified_syns <- function(x,
                                 dark_mode = FALSE,
                                 line_size = 0.9,
                                 dot_size = 0.1,
                                 line_col = "black",
                                 sd_col = "grey80",
                                 condition = NA,
                                 show_plot = TRUE) {
  if (interactive()) message("\nCreating synergy plots for condition ", condition, "...")

  module <- muscle <- time <- value <- variable <- ymax <- ymin <- NULL

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

  # Prepare colours
  if (dark_mode) {
    if (line_col == "black") line_col <- "white"
    bg_col <- "grey12"
    text_col <- "white"
  } else {
    bg_col <- "white"
    text_col <- "black"
  }

  varlist <- list()

  for (syn in 1:max_syns) {

    # To write axis titles
    if (syn == max_syns) {
      axis_title_col <- ifelse(dark_mode, "white", "black")
    } else {
      axis_title_col <- "transparent"
    }

    # Select relevant synergies
    data_M <- lapply(SYNS_M, function(y) {
      muscles <- rownames(y)
      y <- y[, grep(paste0("^Syn", syn), colnames(y))]
      if (length(y) > 0) names(y) <- muscles
      t(y)
    })
    data_P <- lapply(SYNS_P, function(y) t(y[, grep(paste0("^Syn", syn), colnames(y))]))

    # Remove empty trials, if present
    data_M <- data_M[lapply(data_M, length) > 0]
    data_P <- data_P[lapply(data_P, length) > 0]

    if (length(data_P) > 0) {
      # Put modules in a single data frame
      data_M <- plyr::ldply(data_M, data.frame, .id = "trial")
      # Put primitives in a single data frame
      data_P <- plyr::ldply(data_P, data.frame, .id = "trial")

      data_M_av <- data.frame(value = colMeans(data_M[, -1]))
      data_M_av <- data.frame(
        muscle = rownames(data_M_av),
        data_M_av
      )

      data_P_av <- data.frame(
        time = c(1:(ncol(data_P) - 1)),
        value = colMeans(data_P[, -1])
      )
      data_P_sd <- data.frame(
        time = c(1:(ncol(data_P) - 1)),
        ymin = data_P_av$value - apply(data_P[, -1], 2, stats::sd),
        ymax = data_P_av$value + apply(data_P[, -1], 2, stats::sd)
      )

      data_M <- reshape2::melt(data_M, id = "trial")
      data_P <- reshape2::melt(data_P, id = "trial")

      temp_M <- ggplot2::ggplot() +
        ggplot2::ylim(0, 1) +
        ggplot2::geom_bar(
          data = data_M_av,
          ggplot2::aes(x = muscle, y = value),
          fill = line_col, alpha = 0.75,
          stat = "identity"
        ) +
        ggplot2::scale_x_discrete(limits = data_M_av$muscle) +
        ggplot2::geom_jitter(
          data = data_M,
          colour = line_col,
          ggplot2::aes(x = variable, y = value),
          width = dot_size,
          size = dot_size
        ) +
        ggplot2::labs(
          title = paste0("Motor module ", syn),
          x = "Muscle",
          y = "Contribution"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, colour = text_col),
          axis.title.x = ggplot2::element_text(colour = axis_title_col),
          axis.title.y = ggplot2::element_text(colour = axis_title_col),
          axis.text.x = ggplot2::element_text(colour = text_col),
          axis.text.y = ggplot2::element_text(colour = text_col),
          axis.ticks = ggplot2::element_line(colour = "grey"),
          plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          panel.background = ggplot2::element_rect(fill = bg_col, colour = "grey"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(0, 0.2, 0.1, 0.2), "cm"),
          legend.position = "none"
        )

      temp_P <- ggplot2::ggplot() +
        ggplot2::ylim(-0.2, 1.2) +
        ggplot2::geom_ribbon(
          data = data_P_sd,
          ggplot2::aes(x = time, ymin = ymin, ymax = ymax),
          fill = sd_col
        ) +
        ggplot2::geom_line(
          data = data_P_av,
          ggplot2::aes(x = time, y = value),
          colour = line_col, size = line_size
        ) +
        ggplot2::labs(
          title = paste0("Motor primitive ", syn),
          x = "Time",
          y = "Amplitude"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, colour = text_col),
          axis.title.x = ggplot2::element_text(colour = axis_title_col),
          axis.title.y = ggplot2::element_text(colour = axis_title_col),
          axis.text.x = ggplot2::element_text(colour = text_col),
          axis.text.y = ggplot2::element_text(colour = text_col),
          axis.ticks = ggplot2::element_line(colour = "grey"),
          plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          panel.background = ggplot2::element_rect(fill = bg_col, colour = "grey"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(0, 0.2, 0.1, 0.2), "cm"),
          legend.position = "none"
        )
    } else {
      data_P_av <- data.frame(
        time = c(1:points),
        value = -0.2
      )
      data_M_av <- data.frame(
        muscle = muscles,
        value = 0
      )

      temp_M <- ggplot2::ggplot() +
        ggplot2::ylim(0, 1) +
        ggplot2::geom_bar(
          data = data_M_av,
          ggplot2::aes(x = muscle, y = value),
          fill = "transparent",
          stat = "identity"
        ) +
        ggplot2::scale_x_discrete(limits = data_M_av$muscle) +
        ggplot2::labs(
          title = paste0("Motor module ", syn),
          x = "Muscle",
          y = "Contribution"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, colour = text_col),
          axis.title.x = ggplot2::element_text(colour = axis_title_col),
          axis.title.y = ggplot2::element_text(colour = axis_title_col),
          axis.text.x = ggplot2::element_text(colour = text_col),
          axis.text.y = ggplot2::element_text(colour = text_col),
          axis.ticks = ggplot2::element_line(colour = "grey"),
          plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          panel.background = ggplot2::element_rect(fill = bg_col, colour = "grey"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(0, 0.2, 0.1, 0.2), "cm"),
          legend.position = "none"
        )

      temp_P <- ggplot2::ggplot() +
        ggplot2::ylim(-0.2, 1.2) +
        ggplot2::geom_line(
          data = data_P_av,
          ggplot2::aes(x = time, y = value),
          colour = "transparent", size = line_size
        ) +
        ggplot2::labs(
          title = paste0("Motor primitive ", syn),
          x = "Time",
          y = "Amplitude"
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, colour = text_col),
          axis.title.x = ggplot2::element_text(colour = axis_title_col),
          axis.title.y = ggplot2::element_text(colour = axis_title_col),
          axis.text.x = ggplot2::element_text(colour = text_col),
          axis.text.y = ggplot2::element_text(colour = text_col),
          axis.ticks = ggplot2::element_line(colour = "grey"),
          plot.background = ggplot2::element_rect(fill = bg_col, colour = bg_col),
          panel.background = ggplot2::element_rect(fill = bg_col, colour = "grey"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(0, 0.2, 0.1, 0.2), "cm"),
          legend.position = "none"
        )
    }

    varname_P <- paste0("r", 2 * syn)
    varname_M <- paste0("r", 2 * syn - 1)
    varlist[[2 * syn]] <- assign(varname_P, temp_P)
    varlist[[2 * syn - 1]] <- assign(varname_M, temp_M)
  }

  # Arrange plots nicely and return as gtable
  gg <- suppressWarnings(gridExtra::arrangeGrob(
    grobs = varlist,
    nrow = max_syns,
    ncol = 2,
    top = grid::textGrob(paste0("Synergies - ", condition), gp = grid::gpar(col = text_col))
  ))
  # Plot on active graphic device if needed
  if (show_plot) {
    # Prepare graphic device
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(bg = bg_col)
    graphics::plot.new()
    # Arrange
    suppressWarnings(gridExtra::grid.arrange(gg, newpage = FALSE))
  }
  return(gg)
  if (interactive()) message("...done!")
}
