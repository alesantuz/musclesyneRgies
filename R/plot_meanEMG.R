#' Plot EMG averaged across all cycles
#'
#' @param x A data frame containing filtered EMG organised in columns
#' @param trial Character: the name of the considered trial, for archiving purposes
#' @param path_for_graphs Path where plots should be saved
#' @param filetype Plot file type
#' @param width Plot width in pixels
#' @param height Plot height in pixels
#' @param resolution Plot resolution in pixels
#'
#' @details
#' If `path_for_graphs` is not specified, plots will appear in the plot pane and will not be saved.
#'
#' @return Exports average filtered and normalised EMG.
#'
#' @export
#'
#' @examples
#' ## Create "Graphs" folder if it does not exist
#' data_path <- getwd()
#' data_path <- paste0(data_path, .Platform$file.sep)
#' dir.create("Graphs", showWarnings = FALSE)
#' path_for_graphs <- paste0(data_path, "Graphs", .Platform$file.sep)
#'
#' data(FILT_EMG)
#'
#' for (ii in seq_along(FILT_EMG)) {
#'   ## Plot the average filtered and time-normalised EMG
#'   plot_meanEMG(FILT_EMG[[ii]],
#'     trial = names(FILT_EMG)[ii],
#'     path_for_graphs = path_for_graphs
#'   )
#' }
#'
#' ## Check plots in the new folder before running the following (will delete!)
#'
#' ## Delete folder
#' unlink("Graphs", recursive = TRUE)
plot_meanEMG <- function(x,
                         trial,
                         path_for_graphs = NA,
                         filetype = "png",
                         width = 2000,
                         height = 1875,
                         resolution = 140) {
  if (!inherits(x, "data.frame")) {
    stop("Objects are not data frames")
  } else {
    # Muscle list, including time
    muscles <- colnames(x)
  }

  time <- signal <- NULL

  # Calculate mean cycles
  x <- stats::aggregate(x, by = list(x[, "time"]), FUN = mean)
  x[, grep("^Group.+", colnames(x))] <- NULL
  points <- max(x[, "time"])
  # Normalise mean cycles
  x <- data.frame(apply(x, 2, function(y) y / max(y)))
  x[, "time"] <- c(1:points)

  if (!is.na(path_for_graphs)) {
    Cairo::Cairo(
      file = paste0(path_for_graphs, trial, ".", filetype),
      width = width, height = height, dpi = resolution
    )
  }

  varlist <- list()

  for (mm in 2:length(muscles)) {
    data <- data.frame(
      time = x$time,
      signal = x[, grep(paste0("^", muscles[mm], "$"), colnames(x))]
    )

    varname <- paste("pp", mm, sep = "")

    temp <- ggplot2::ggplot() +
      ggplot2::ggtitle(muscles[mm]) +
      ggplot2::ylim(0, 1) +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes(x = time, y = signal),
        colour = "black", size = 0.9
      ) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "white", colour = "grey"),
        panel.grid.major = ggplot2::element_line(colour = "grey", size = 0.05),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "none"
      )

    varlist[[mm - 1]] <- assign(varname, temp)
  }

  gridExtra::grid.arrange(
    grobs = varlist,
    nrow = ceiling(sqrt(length(varlist))),
    ncol = ceiling(sqrt(length(varlist))),
    top = (paste0(trial, sep = ""))
  )

  if (!is.na(path_for_graphs)) grDevices::dev.off()
}
