#' Plot raw EMG
#'
#' @param x Object of class `EMG` with elements `cycles` and `emg`
#' @param trial Character: the name of the considered trial, for archiving purposes
#' @param plot_time How many seconds of data should be plotted?
#' @param start At which data point should the plot start?
#' @param path_for_graphs Path where plots should be saved
#' @param filetype Plot file type
#' @param width Plot width in pixels
#' @param height Plot height in pixels, for each muscle
#' @param resolution Plot resolution in pixels
#'
#' @details
#' If `path_for_graphs` is not specified, plots will appear in the plot pane and will not be saved.
#'
#' @return Exports raw EMG plots of the specified length.
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
#' data(RAW_DATA)
#'
# for (ii in seq_along(RAW_DATA)) {
#   ## Plot the first three seconds of each trial
#   plot_rawEMG(RAW_DATA[[ii]],
#               trial=names(RAW_DATA)[ii],
#               path_for_graphs=path_for_graphs)
# }
#'
#' ## Check plots in the new folder before running the following (will delete!)
#'
#' ## Delete folder
#' unlink("Graphs", recursive = TRUE)
plot_rawEMG <- function(x,
                        trial,
                        plot_time = 3,
                        start = 1,
                        path_for_graphs = NA,
                        filetype = "png",
                        width = 2000,
                        height = 500,
                        resolution = 280) {
  if (!inherits(x, "EMG")) {
    stop("Object is not of class EMG, please create objects in the right format with \"rawdata\"")
  } else {
    x <- x$emg
  }

  time <- signal <- NULL

  # Muscle list, including time
  muscles <- colnames(x)
  # EMG system acquisition frequency [Hz]
  freq <- round(1 / (mean(diff(x[, "time"]), na.rm = T)), 0)

  start <- 1
  stop <- freq * plot_time + start - 1
  if (start <= 0) start <- 1

  if (!is.na(path_for_graphs)) {
    Cairo::Cairo(
      file = paste0(path_for_graphs, trial, ".", filetype),
      width = width, height = height * (length(muscles) - 1), dpi = resolution
    )
  }

  varlist <- list()

  for (mm in 2:length(muscles)) {
    data <- data.frame(
      time = x[start:stop, "time"],
      signal = x[start:stop, grep(paste0("^", muscles[mm], "$"), colnames(x))]
    )

    data$signal <- data$signal / max(data$signal, na.rm = T)
    data$signal <- data$signal / min(data$signal, na.rm = T)

    varname <- paste("pp", mm, sep = "")

    temp <- ggplot2::ggplot() +
      ggplot2::ggtitle(muscles[mm]) +
      ggplot2::ylim(-1, 1) +
      ggplot2::geom_line(
        data = data,
        ggplot2::aes(x = time, y = signal),
        colour = "black", size = 0.3
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
    nrow = length(varlist),
    ncol = 1,
    top = (paste0(trial, sep = ""))
  )

  if (!is.na(path_for_graphs)) grDevices::dev.off()
}
