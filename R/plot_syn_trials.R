#' Plot muscle synergies (individual trials)
#'
#' @param x Object of class `musclesyneRgies`
#' @param max_syns Maximum number of synergies to be plotted
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
#' @return Plots of the unclassified synergies, trial by trial.
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
#' data(SYNS)
#'
#' max_syns <- max(unlist(lapply(SYNS, function(x) x$syns)))
#'
#' ## Plot the first two trials in the list
#' for (ii in c(1, 2)) {
#'   ## Plot the unclassified synergies, trial-by-trial
#'   plot_syn_trials(SYNS[[ii]],
#'     max_syns = max_syns,
#'     trial = names(SYNS)[ii],
#'     path_for_graphs = path_for_graphs
#'   )
#' }
#'
#' ## Check plots in the new folder before running the following (will delete!)
#'
#' ## Delete folder
#' unlink("Graphs", recursive = TRUE)
plot_syn_trials <- function(x,
                            max_syns,
                            trial,
                            path_for_graphs = NA,
                            filetype = "png",
                            width = 2000,
                            height = 500 * max_syns,
                            resolution = 280) {
  if (!inherits(x, "musclesyneRgies")) {
    stop("Object is not of class musclesynergies, please create objects in the right format with \"synsNMF\"")
  } else {
    # Get motor modules and concatenated motor primitives
    M <- x$M
    P <- x$P
  }

  ymin <- ymax <- value <- muscle <- NULL

  time <- P$time
  P$time <- NULL
  points <- max(time)

  if (!is.na(path_for_graphs)) {
    Cairo::Cairo(
      file = paste0(path_for_graphs, trial, ".", filetype),
      type = filetype, width = width, height = height, pointsize = 20, dpi = resolution
    )
  }

  varlist <- list()

  for (syn in 1:max_syns) {
    if (syn <= ncol(M)) {

      # Select relevant synergies
      data_P <- data.frame(time,
        value = P[, syn]
      )
      data_M <- data.frame(
        muscle = rownames(M),
        value = M[, syn]
      )

      # Normalise primitive
      data_P$value <- data_P$value / max(data_P$value)

      # Organise primitives one cycle per row
      data_P_temp <- matrix(0, nrow = nrow(data_P) / points, ncol = points)
      for (cycle in 1:nrow(data_P_temp)) {
        start <- (cycle - 1) * points + 1
        stop <- start + points - 1
        P_temp <- data_P$value[start:stop]
        data_P_temp[cycle, ] <- P_temp
      }

      # Calculate mean and standard deviation of each cycle's primitive
      data_P_av <- data.frame(time,
        value = colMeans(data_P_temp)
      )
      data_P_sd <- data.frame(time,
        ymin = data_P_av$value - apply(data_P_temp, 2, stats::sd),
        ymax = data_P_av$value + apply(data_P_temp, 2, stats::sd)
      )

      temp_P <- ggplot2::ggplot() +
        ggplot2::ggtitle(paste0("Motor primitive ", syn)) +
        ggplot2::ylim(-0.2, 1.2) +
        ggplot2::geom_ribbon(
          data = data_P_sd,
          ggplot2::aes(x = time, ymin = ymin, ymax = ymax),
          fill = "grey80"
        ) +
        ggplot2::geom_line(
          data = data_P_av,
          ggplot2::aes(x = time, y = value),
          colour = "black", size = 0.9
        ) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = "white", colour = "grey"),
          panel.grid.major = ggplot2::element_line(colour = "grey", size = 0.05),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "none"
        )

      temp_M <- ggplot2::ggplot() +
        ggplot2::ggtitle(paste0("Motor module ", syn)) +
        ggplot2::ylim(0, 1) +
        ggplot2::geom_hline(yintercept = c(0.25, 0.5, 0.75, 1), size = 0.05, color = "grey70") +
        ggplot2::geom_bar(
          data = data_M,
          ggplot2::aes(x = muscle, y = value),
          fill = "black", alpha = 0.3,
          stat = "identity"
        ) +
        ggplot2::scale_x_discrete(limits = data_M$muscle) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          panel.background = ggplot2::element_rect(fill = "white", colour = "grey"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          legend.position = "none"
        )
    } else {
      temp_P <- NULL
      temp_M <- NULL
    }

    varname_P <- paste0("r", 2 * syn)
    varname_M <- paste0("r", 2 * syn - 1)
    varlist[[2 * syn]] <- assign(varname_P, temp_P)
    varlist[[2 * syn - 1]] <- assign(varname_M, temp_M)
  }

  suppressWarnings(gridExtra::grid.arrange(
    grobs = varlist, nrow = max_syns, ncol = 2,
    top = (paste0("Synergies - ", trial))
  ))

  if (!is.na(path_for_graphs)) grDevices::dev.off()
}
