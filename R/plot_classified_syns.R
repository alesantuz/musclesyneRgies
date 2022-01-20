#' Plot muscle synergies
#'
#' @param x List of objects of class `musclesyneRgies` (must be classified)
#' @param path_for_graphs Path where plots should be saved
#' @param condition Character: the condition that is being analysed, for archiving purposes
#' @param filetype Plot file type
#' @param width Plot width in pixels
#' @param height Plot height in pixels
#' @param resolution Plot resolution in pixels
#'
#' @details
#' If `path_for_graphs` is not specified, plots will appear in the plot pane and will not be saved.
#'
#' @return Global plot containing the average classified muscle synergies and individual trials
#' (motor modules) or standard deviations (motor primitives)
#'
#' @export
#'
#' @examples
#' ## Create "Graphs" folder if it does not exist
#' data_path  <- getwd()
#' data_path  <- paste0(data_path, .Platform$file.sep)
#' dir.create("Graphs", showWarnings=FALSE)
#' path_for_graphs <- paste0(data_path, "Graphs", .Platform$file.sep)
#'
#' data(SYNS)
#'
#' ## Classify synergies with k-means
#' SYNS_classified <- classify_kmeans(SYNS,
#'                                    path_for_graphs=path_for_graphs,
#'                                    interactive=FALSE)
#'
#' ## Export plots
#' plot_classified_syns(SYNS_classified,
#'                      path_for_graphs=path_for_graphs,
#'                      condition="TW")
#'
#' ## Check plots in the new folder before running the following (will delete!)
#'
#' ## Delete folder
#' unlink("Graphs", recursive=TRUE)

plot_classified_syns <- function(x,
                                 path_for_graphs = NA,
                                 condition,
                                 filetype = "png",
                                 width = 1800,
                                 height = 625,
                                 resolution = 280) {

  message("\nSaving synergy plots for condition ", condition, "...")

  time <- ymin <- ymax <- value <- muscle <- variable <- NULL

  # Get classification method
  class_method <- unique(unlist(lapply(x, function(y) y$classification)))

  if (class_method=="none" || length(class_method)>1) {
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

    if (ncol(y)>0) {
      temp   <- matrix(0, nrow=points, ncol=ncol(y))

      colnames(temp) <- colnames(y)

      for (cc in seq(1, (1+nrow(y)-points), points)) {
        temp <- temp+y[c(cc:(cc+points-1)), ]
      }

      # Divide by the number of cycles to get mean value
      temp <- temp/(nrow(y)/points)

      # Minimum subtraction
      x <- apply(temp, 2, function(z) z-min(z))
      # Amplitude normalisation
      x <- apply(temp, 2, function(z) z/max(z))
    }

    return(data.frame(x))
  })

  points  <- unique(unlist(lapply(SYNS_P, function(y) nrow(y))))
  muscles <- unique(unlist(lapply(SYNS_M, function(y) rownames(y))))

  if (!is.na(path_for_graphs)) {
    Cairo::Cairo(file=paste0(path_for_graphs, "SYNS_",
                             condition, "_",
                             class_method, "_classification.",
                             filetype),
                 type=filetype, width=width, height=height*max_syns, pointsize=20, dpi=resolution)
  }

  varlist <- list()

  for (syn in 1:max_syns) {

    # Select relevant synergies
    data_P <- lapply(SYNS_P, function(y) t(y[, grep(paste0("^Syn", syn), colnames(y))]))
    data_M <- lapply(SYNS_M, function(y) {
      muscles <- rownames(y)
      y <- y[, grep(paste0("^Syn", syn), colnames(y))]
      if (length(y)>0) names(y) <- muscles
      t(y)
    })

    # Remove empty trials, if present
    data_P <- data_P[lapply(data_P, length)>0]
    data_M <- data_M[lapply(data_M, length)>0]

    if (length(data_P)>0) {
      # Put primitives in a single data frame
      data_P <- plyr::ldply(data_P, data.frame, .id="trial")
      # Put modules in a single data frame
      data_M <- plyr::ldply(data_M, data.frame, .id="trial")

      data_P_av <- data.frame(time=c(1:(ncol(data_P)-1)),
                              value=colMeans(data_P[, -1]))
      data_P_sd <- data.frame(time=c(1:(ncol(data_P)-1)),
                              ymin=data_P_av$value-apply(data_P[, -1], 2, stats::sd),
                              ymax=data_P_av$value+apply(data_P[, -1], 2, stats::sd))

      data_M_av <- data.frame(value=colMeans(data_M[, -1]))
      data_M_av <- data.frame(muscle=rownames(data_M_av),
                              data_M_av)

      data_P <- reshape2::melt(data_P, id="trial")
      data_M <- reshape2::melt(data_M, id="trial")

      temp_P <- ggplot2::ggplot() +
        ggplot2::ggtitle(paste0("Motor primitive ", syn)) +
        ggplot2::ylim(-0.2, 1.2) +
        ggplot2::geom_ribbon(data=data_P_sd,
                             ggplot2::aes(x=time, ymin=ymin, ymax=ymax),
                             fill="grey80") +
        ggplot2::geom_line(data=data_P_av,
                           ggplot2::aes(x=time, y=value),
                           colour="black", size=0.9) +
        ggplot2::theme(axis.title=ggplot2::element_blank(),
                       panel.background=ggplot2::element_rect(fill="white", colour="grey"),
                       panel.grid.major=ggplot2::element_line(colour="grey", size=0.05),
                       panel.grid.minor=ggplot2::element_blank(),
                       legend.position="none")

      temp_M <- ggplot2::ggplot() +
        ggplot2::ggtitle(paste0("Motor module ", syn)) +
        ggplot2::ylim(0, 1) +
        ggplot2::geom_hline(yintercept=c(0.25, 0.5, 0.75, 1), size=0.05, color="grey70") +
        ggplot2::geom_bar(data=data_M_av,
                          ggplot2::aes(x=muscle, y=value),
                          fill="black", alpha=0.3,
                          stat="identity") +
        ggplot2::scale_x_discrete(limits=data_M_av$muscle) +
        ggplot2::geom_jitter(data=data_M,
                             ggplot2::aes(x=variable, y=value),
                             fill="black", width=0.1, size=0.1) +
        ggplot2::theme(axis.title=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),
                       panel.background=ggplot2::element_rect(fill="white", colour="grey"),
                       panel.grid.major=ggplot2::element_blank(),
                       panel.grid.minor=ggplot2::element_blank(),
                       axis.ticks=ggplot2::element_blank(),
                       legend.position="none")
    } else {
      data_P_av <- data.frame(time=c(1:points),
                              value=-0.2)
      data_M_av <- data.frame(muscle=muscles,
                              value=0)

      temp_P <- ggplot2::ggplot() +
        ggplot2::ggtitle(paste0("Motor primitive ", syn)) +
        ggplot2::ylim(-0.2, 1.2) +
        ggplot2::geom_line(data=data_P_av,
                           ggplot2::aes(x=time, y=value),
                           colour="transparent") +
        ggplot2::theme(axis.title=ggplot2::element_blank(),
                       panel.background=ggplot2::element_rect(fill="white", colour="grey"),
                       panel.grid.major=ggplot2::element_line(colour="grey", size=0.05),
                       panel.grid.minor=ggplot2::element_blank(),
                       legend.position="none")

      temp_M <- ggplot2::ggplot() +
        ggplot2::ggtitle(paste0("Motor module ", syn)) +
        ggplot2::ylim(0, 1) +
        ggplot2::geom_hline(yintercept=c(0.25, 0.5, 0.75, 1), size=0.05, color="grey70") +
        ggplot2::geom_bar(data=data_M_av,
                          ggplot2::aes(x=muscle, y=value),
                          fill="transparent", alpha=0.3,
                          stat="identity") +
        ggplot2::scale_x_discrete(limits=data_M_av$muscle) +
        ggplot2::theme(axis.title=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),
                       panel.background=ggplot2::element_rect(fill="white", colour="grey"),
                       panel.grid.major=ggplot2::element_blank(),
                       panel.grid.minor=ggplot2::element_blank(),
                       axis.ticks=ggplot2::element_blank(),
                       legend.position="none")
    }

    varname_P <- paste0("r", 2*syn)
    varname_M <- paste0("r", 2*syn-1)
    varlist[[2*syn]]   <- assign(varname_P, temp_P)
    varlist[[2*syn-1]] <- assign(varname_M, temp_M)

  }

  suppressWarnings(gridExtra::grid.arrange(grobs=varlist, nrow=max_syns, ncol=2,
                                           top=(paste0("Synergies - ", condition))))

  if (!is.na(path_for_graphs)) grDevices::dev.off()

  message("...done!")

}
