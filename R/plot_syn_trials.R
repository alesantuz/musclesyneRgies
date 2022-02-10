#' Plot muscle synergies (individual trials)
#'
#' @param x Object of class `musclesyneRgies`
#' @param max_syns Number of synergies to be plotted or how many rows should the final panel
#' be divided into
#' @param trial Character: the name of the considered trial, for archiving purposes
#' @param dark_mode To enable dark mode
#' @param line_size Line thickness
#' @param line_col Line colour
#' @param sd_col Standard deviation ribbon colour
#' @param show_plot Logical, to decide whether plots should be plotted in the active graphic device
#'
#' @details
#' If `show_plot` is TRUE (default) plots are also shown in the active graphic device.
#' Plots can then be saved with the preferred export method, such as `ggplot2::ggsave`.
#'
#' @return Plots of the unclassified synergies, trial by trial.
#'
#' @export
#'
#' @examples
#' # Load some data
#' data(SYNS)
#'
#' # Find maximum number of synergies
#' max_syns <- max(unlist(lapply(SYNS, function(x) x$syns)))
#'
#' # Save a plot with the first (and only, in this case) trial in the list
#' pp <- plot_syn_trials(SYNS[[1]],
#'   max_syns = max_syns,
#'   trial = names(SYNS)[1],
#'   dark_mode = TRUE,
#'   line_size = 0.8,
#'   line_col = "tomato1",
#'   sd_col = "tomato4",
#'   show_plot = FALSE
#' )
plot_syn_trials <- function(x,
                            max_syns,
                            trial,
                            dark_mode = FALSE,
                            line_size = 0.6,
                            line_col = "black",
                            sd_col = "grey80",
                            show_plot = TRUE) {
  if (!inherits(x, "musclesyneRgies")) {
    stop("Object is not of class musclesynergies, please create objects in the right format with \"synsNMF\"")
  } else {
    # Get motor modules and concatenated motor primitives
    M <- x$M
    P <- x$P
  }

  Group.1 <- ymin <- ymax <- primitive <- muscle <- module <- NULL

  time <- P$time
  P$time <- NULL

  # Normalise and calculate mean primitives, removing Group and time columns
  P_av <- apply(P, 2, function(y) y / max(y)) |>
    stats::aggregate(by = list(time), FUN = mean) |>
    subset(select = -Group.1)

  # Calculate standard deviation for plot
  P_sd <- apply(P, 2, function(y) y / max(y)) |>
    stats::aggregate(by = list(time), FUN = stats::sd) |>
    subset(select = -Group.1)

  if (all(is.na(P_sd))) P_sd[is.na(P_sd)] <- 0

  # Prepare colours
  if (dark_mode) {
    if (line_col == "black") line_col <- "white"
    bg_col <- "grey12"
    text_col <- "white"
  } else {
    bg_col <- "white"
    text_col <- "black"
  }

  # Create plots
  # Motor modules
  varlist_M <- lapply(colnames(M), function(y) {
    # Gather motor modules
    M_data <- data.frame(
      muscle = rownames(M),
      module = M[, y]
    )

    # To write axis titles
    if (y == colnames(M)[length(colnames(M))]) {
      axis_title_col <- ifelse(dark_mode, "white", "black")
    } else {
      axis_title_col <- "transparent"
    }

    mm <- ggplot2::ggplot(M_data) +
      ggplot2::ylim(0, 1) +
      ggplot2::geom_bar(
        ggplot2::aes(x = muscle, y = module),
        fill = line_col, alpha = 0.75,
        stat = "identity"
      ) +
      ggplot2::scale_x_discrete(limits = M_data$muscle) +
      ggplot2::labs(
        title = paste0("Motor module ", gsub("Syn", "", y)),
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
    return(mm)
  })

  # Motor primitives
  varlist_P <- lapply(colnames(P), function(y) {
    # Gather mean motor primitive and standard deviation
    P_data <- data.frame(
      time,
      primitive = P_av[, y],
      ymin = P_av[, y] - P_sd[, y],
      ymax = P_av[, y] + P_sd[, y],
      row.names = NULL
    )

    # To write axis titles
    if (y == colnames(P)[length(colnames(P))]) {
      axis_title_col <- ifelse(dark_mode, "white", "black")
    } else {
      axis_title_col <- "transparent"
    }

    pp <- ggplot2::ggplot(P_data) +
      ggplot2::ylim(-0.2, 1.2) +
      ggplot2::geom_ribbon(
        ggplot2::aes(x = time, ymin = ymin, ymax = ymax),
        fill = sd_col
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = time, y = primitive),
        colour = line_col, size = line_size
      ) +
      ggplot2::labs(
        title = paste0("Motor primitive ", gsub("Syn", "", y)),
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
    return(pp)
  })

  # Arrange plots nicely and return as gtable
  names(varlist_M) <- seq(1, 2 * length(varlist_M), 2)
  names(varlist_P) <- seq(2, 2 * length(varlist_M), 2)
  varlist <- c(varlist_M, varlist_P)[order(names(c(varlist_M, varlist_P)))]

  gg <- gridExtra::arrangeGrob(
    grobs = varlist,
    nrow = max_syns,
    ncol = 2,
    top = grid::textGrob(paste0("Trial: ", trial), gp = grid::gpar(col = text_col))
  )
  # Plot on active graphic device if needed
  if (show_plot) {
    # Prepare graphic device
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(bg = bg_col)
    graphics::plot.new()
    # Arrange
    gridExtra::grid.arrange(gg, newpage = FALSE)
  }
  return(gg)
}
