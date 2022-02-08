#' Plot raw EMG
#'
#' @param x Object of class `EMG` with elements `cycles` and `emg`
#' @param trial Character: the name of the considered trial, for archiving purposes
#' @param plot_time How many seconds of data should be plotted?
#' @param start At which data point should the plot start?
#' @param row_number How many rows should the final plot be divided into?
#' @param col_number How many columns should the final plot be divided into?
#' @param dark_mode To enable dark mode
#' @param line_size Line thickness
#' @param line_col Line colour
#' @param show_plot Logical, to decide whether plots should be plotted in the active graphic device
#'
#' @details
#' If `show_plot` is TRUE (default) plots are also shown in the active graphic device.
#' Plots can then be saved with the preferred export method, such as `ggplot2::ggsave`.
#'
#' @return Plots raw EMG trials of the specified length.
#'
#' @export
#'
#' @examples
#' # Load some data
#' data(RAW_DATA)
#'
#' # Plot first (and only) trial in RAW_DATA, first three seconds, in dark mode
#' plot_rawEMG(RAW_DATA[[1]],
#'   trial = names(RAW_DATA)[1],
#'   row_number = 4,
#'   col_number = 4,
#'   dark_mode = TRUE,
#'   line_col = "tomato3"
#' )
plot_rawEMG <- function(x,
                        trial,
                        plot_time = 3,
                        start = 1,
                        row_number = NA,
                        col_number = 1,
                        dark_mode = FALSE,
                        line_size = 0.3,
                        line_col = "black",
                        show_plot = TRUE) {
  if (!inherits(x, "EMG")) {
    stop("Object is not of class EMG, please create objects in the right format with \"rawdata\"")
  } else {
    x <- x$emg
  }

  signal <- NULL

  # EMG system acquisition frequency [Hz]
  freq <- round(1 / (mean(diff(x$time), na.rm = T)), 0)

  # Calculate starting and stopping point for the subset
  stop <- freq * plot_time + start - 1
  if (start <= 0) start <- 1

  # Put time info aside
  time <- x$time[start:stop]

  # Create data subset and remove time column
  x <- x[start:stop, ] |>
    subset(select = -time)

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
  varlist <- lapply(colnames(x), function(y) {
    muscle_data <- data.frame(time, signal = x[, y])

    # To write axis titles
    if (y == colnames(x)[length(colnames(x))]) {
      axis_title_col <- ifelse(dark_mode, "white", "black")
    } else {
      axis_title_col <- "transparent"
    }

    # To centre data in plot
    y_max <- max(
      abs(min(muscle_data$signal, na.rm = TRUE)),
      max(muscle_data$signal, na.rm = TRUE)
    )

    gg <- ggplot2::ggplot() +
      ggplot2::ylim(y_max, -y_max) +
      ggplot2::geom_line(
        data = muscle_data,
        ggplot2::aes(x = time, y = signal),
        colour = line_col, size = line_size
      ) +
      ggplot2::labs(title = y, x = "Time", y = "Amplitude") +
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
    return(gg)
  })

  # Check for consistency of row and column number
  if (is.na(row_number)) row_number <- ceiling(length(colnames(x)) / col_number)

  # Arrange plots nicely and return as gtable
  gg <- gridExtra::arrangeGrob(
    grobs = varlist,
    nrow = row_number,
    ncol = col_number,
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
