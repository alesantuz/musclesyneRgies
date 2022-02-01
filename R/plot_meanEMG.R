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
  }

  signal <- NULL

  # Put time info aside
  time <- c(1:max(x[, "time"]))

  # Calculate mean cycles and normalise them, remove Group and time columns
  x <- stats::aggregate(x, by = list(x[, "time"]), FUN = mean)[-1] |>
    subset(select = -time) |>
    apply(2, function(y) y / max(y)) |>
    as.data.frame()

  if (!is.na(path_for_graphs)) {
    Cairo::Cairo(
      file = paste0(path_for_graphs, trial, ".", filetype),
      width = width, height = height, dpi = resolution
    )
  }

  # Create plots
  varlist <- lapply(colnames(x), function(y) {
    muscle_data <- data.frame(time, signal = x[, y])

    gg <- ggplot2::ggplot() +
      ggplot2::ggtitle(y) +
      ggplot2::ylim(0, 1) +
      ggplot2::geom_line(
        data = muscle_data,
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
    return(gg)
  })

  gridExtra::grid.arrange(
    grobs = varlist,
    nrow = ceiling(sqrt(length(varlist))),
    ncol = ceiling(sqrt(length(varlist))),
    top = (paste0(trial, sep = ""))
  )

  if (!is.na(path_for_graphs)) grDevices::dev.off()
}
