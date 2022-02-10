#' Muscle synergy classification with k-means
#'
#' @param x A list of `musclesyneRgies` objects
#' @param MSE_lim Mean squared error threshold for determining the minimum number of clusters
#' @param inspect Logical, ask for interactive re-ordering or go fully automated?
#' @param show_plot Logical, to decide whether plots should be plotted in the active graphic device
#'
#' @details
#' This function must be applied to a list with a sufficient amount of trials, otherwise the
#' classification will not work. Typically, at least 10 trials for the same condition are needed
#' for satisfactory classification. If `show_plot` is TRUE (default) plots are also shown in the active graphic device.
#' Plots can then be saved with the preferred export method, such as `ggplot2::ggsave`.
#'
#' @return
#' List of `musclesyneRgies` objects, each with elements:\cr
#' - `syns` factorisation rank or minimum number of synergies\cr
#' - `M` motor modules (time-invariant coefficients)\cr
#' - `P` motor primitives (time-dependent coefficients)\cr
#' - `V` original data\cr
#' - `Vr` reconstructed data\cr
#' - `iterations` number of iterations to convergence\cr
#' - `R2` quality of reconstruction (coefficient of determination)
#' - `rank_type` was the rank `fixed` or `variable`?\cr
#' - `classification` classification type (`k-means`)
#'
#' @export
#'
#' @examples
#' # Load some data
#' data(SYNS)
#' # Classify synergies
#' SYNS_classified <- classify_kmeans(SYNS)
classify_kmeans <- function(x,
                            MSE_lim = 1e-03,
                            inspect = FALSE,
                            show_plot = FALSE) {
  FWHM_P <- CoA_P <- clusters_M <- clusters_P <- NULL

  # Get motor modules and concatenated motor primitives
  M <- lapply(x, function(y) y$M)
  P <- lapply(x, function(y) y$P)

  # Make sure that all motor primitives are normalised to the same amount of points
  points <- unlist(lapply(P, function(y) max(y$time)))

  if (stats::sd(points) != 0) {
    stop(
      "\nNot all motor primitives are normalised to the same amount of points!",
      "\nPlease re-check your data\n"
    )
  } else {
    points <- unique(points)
  }

  # Find number of muscles
  muscle_num <- unique(unlist(lapply(M, function(y) nrow(y))))
  if (length(muscle_num) != 1) {
    stop("Not all trials have the same number of muscles!!!")
  }

  if (interactive()) message("\nCalculating mean gait cycles...")

  P <- lapply(P, function(y) {
    y$time <- NULL
    temp <- matrix(0, nrow = points, ncol = ncol(y))

    for (cc in seq(1, (1 + nrow(y) - points), points)) {
      temp <- temp + y[c(cc:(cc + points - 1)), ]
    }

    # Divide by the number of cycles to get mean value
    temp <- temp / (nrow(y) / points)

    # Amplitude normalisation
    y <- apply(temp, 2, function(z) z / (max(z)))

    # Transpose to facilitate visualisation
    return(t(y))
  })
  if (interactive()) message("...done!")

  if (interactive()) message("\nPutting primitives into a single data frame...")
  data_P <- plyr::ldply(P, function(y) data.frame(y))
  if (interactive()) message("...done!")

  if (interactive()) message("\nPutting modules into a single data frame...")
  data_M <- plyr::ldply(M, function(y) t(data.frame(y)))
  if (interactive()) message("...done!")

  # Check if names are the same for primitives and modules
  if (identical(data_P$.id, data_M$.id)) {
    trials <- data_M$.id
  } else {
    stop("The names of primitives and modules are not the same!!!")
  }

  # Give names to trials (start from synergy zero because
  # the function "make.unique" works like that)
  # Find non-duplicated names and assign "Syn0" to them
  syn0 <- which(!duplicated(trials))
  trials <- make.unique(trials)
  trials[syn0] <- paste0(trials[syn0], "_Syn0")
  # Assign incremental Syn number to other names
  trials <- gsub("\\.", "_Syn", trials)
  # Start from Syn1 instead that from Syn0
  temp1 <- gsub("[0-9]$", "", trials)
  temp2 <- as.numeric(gsub(".*_Syn", "", trials)) + 1
  trials <- paste0(temp1, temp2)
  # Assign new names to row names and remove id column
  rownames(data_P) <- trials
  rownames(data_M) <- trials
  data_P$.id <- NULL
  data_M$.id <- NULL

  # Filter primitives to improve classification
  data_P <- t(apply(data_P, 1, function(y) {

    # Build filter
    LP <- signal::butter(4, 10 / (points / 2), type = "low")
    # Apply filter
    y <- signal::filtfilt(LP, t(y))

    # Remove negative entries
    y[y < 0] <- 0
    # Subtract the minimum
    y <- y - min(y)
    # Set zeroes to smallest non-negative entry
    temp <- y
    temp[temp == 0] <- Inf
    y[y == 0] <- min(temp, na.rm = T)
    # Normalise to maximum
    y <- y / max(y)

    return(y)
  }))
  data_M <- as.matrix(data_M)

  # Determine number of clusters by computing the within-group sum of squares
  # for an increasing number of clusters and then searching for an elbow in the
  # clusters vs. withinss curve
  # nstart is set >1 due to instabilities found (of course this slows down computation)
  kmeans_all <- list()
  for (clust in 1:muscle_num) {
    kmeans_all[[clust]] <- stats::kmeans(data_P,
      centers = clust,
      nstart = 20,
      algorithm = "Hartigan-Wong"
    )
  }

  withinss <- unlist(lapply(kmeans_all, function(y) sum(y$withinss)))
  withinss <- withinss - min(withinss)
  withinss <- withinss / max(withinss)

  # Find the elbow in the clusters vs. withinss curve
  MSE <- 100 # Initialise the Mean Squared Error (MSE)
  iter <- 0 # Initialise iterations
  while (MSE > MSE_lim) {
    iter <- iter + 1
    if (iter == muscle_num - 1) {
      break
    }
    withinss_interp <- data.frame(
      xx = c(1:(muscle_num - iter + 1)),
      yy = withinss[iter:(muscle_num)]
    )

    linear <- stats::lm(yy ~ xx, withinss_interp)$fitted.values
    MSE <- sum((linear - withinss_interp$yy)^2) / nrow(withinss_interp)
  }
  clust <- iter
  clust_P <- kmeans_all[[clust]]

  # Write number of clusters in a simple way
  clust_num_P <- max(clust_P$cluster)

  # Apply k-means to motor modules
  # Use previously-determined number of clusters
  clust_M <- stats::kmeans(data_M,
    centers = clust_num_P,
    nstart = 20,
    algorithm = "Hartigan-Wong"
  )

  orders <- data.frame(
    clusters_P = clust_P$cluster,
    clusters_M = clust_M$cluster,
    FWHM_P = apply(data_P, 1, function(y) musclesyneRgies::FWHM(y)),
    CoA_P = apply(data_P, 1, function(z) musclesyneRgies::CoA(z))
  )

  clust_num <- clust_num_P

  # Arrange primitive- and module-based clusters in the same order
  temp_P <- subset(orders, select = -c(clusters_M))
  temp_M <- subset(orders, select = -c(clusters_P))
  # Take average FWHM and CoA based on cluster
  geoms_P <- data.frame(stats::aggregate(FWHM_P ~ clusters_P, temp_P, mean),
    CoA_P = stats::aggregate(CoA_P ~ clusters_P, temp_P, mean)$CoA_P
  )
  geoms_M <- data.frame(stats::aggregate(FWHM_P ~ clusters_M, temp_M, mean),
    CoA_P = stats::aggregate(CoA_P ~ clusters_M, temp_M, mean)$CoA_P
  )
  # Define score as sum of FWHM and CoA and normalise to number of points
  geoms_P <- data.frame(
    clust_P = geoms_P$clusters_P,
    score = geoms_P$FWHM_P * geoms_P$CoA_P
  )
  geoms_M <- data.frame(
    clust_M = geoms_M$clusters_M,
    score = geoms_M$FWHM_P * geoms_M$CoA_P
  )

  # Calculate mutual score squared residuals and find minimum
  perms <- gtools::permutations(nrow(geoms_P), r = 2, repeats.allowed = T)
  resids <- numeric()
  for (perm in 1:nrow(perms)) {
    resids[perm] <- (geoms_P$score[perms[perm, 1]] - geoms_M$score[perms[perm, 2]])^2
  }
  perms <- perms[sort(resids, decreasing = F, index.return = T)$ix, ]
  perms <- data.frame(perms[-which(duplicated(perms[, 1])), ])
  colnames(perms) <- c("old", "new")

  if (all(!duplicated(perms$old)) && all(!duplicated(perms$new))) {
    if (!identical(perms$old, perms$new)) {
      temp_M <- orders$clusters_M
      orders$clusters_M <- c(perms$old, temp_M)[match(temp_M, c(perms$new, temp_M))]
    }

    # Find discordant classifications and label as "combined"
    discordant <- which(orders$clusters_P != orders$clusters_M)
    # But first check if all clusters are there
    clust_test <- orders$clusters_P
    clust_test <- unique(clust_test[-discordant])

    if (length(clust_test) < clust_num) {
      if (interactive()) {
        message(
          "\nATTENTION: primitive- and module-based classification don't match!",
          "\nModule-based classification will be discarded!"
        )
      }
      orders$clusters_M <- orders$clusters_P
    } else {
      orders$clusters_P[discordant] <- "combined"
      orders$clusters_M[discordant] <- "combined"
    }
  } else if (all(!duplicated(perms$old)) && any(!duplicated(perms$new))) {
    if (interactive()) {
      message(
        "\nATTENTION: primitive- and module-based classification don't match!",
        "\nModule-based classification will be discarded!"
      )
    }

    orders$clusters_M <- orders$clusters_P
  }

  # Calculate mean primitives and then order using CoA
  mean_P <- matrix(0, nrow = clust_num, ncol = points)
  mean_M <- matrix(0, nrow = clust_num, ncol = muscle_num)
  temp_P <- data_P
  temp_M <- data_M

  # Remove old row names and replace with new orders
  rownames(temp_P) <- gsub("[0-9]*$", "", rownames(temp_P))
  rownames(temp_M) <- gsub("[0-9]*$", "", rownames(temp_M))
  rownames(temp_P) <- paste0(rownames(temp_P), orders$clusters_P)
  rownames(temp_M) <- paste0(rownames(temp_M), orders$clusters_M)

  # Remove combined, if present
  if (any(grepl("combined", rownames(temp_P)))) {
    temp_P <- temp_P[-grep("combined", rownames(temp_P)), ]
    temp_M <- temp_M[-grep("combined", rownames(temp_M)), ]
  }

  # Calculate means
  for (clust in 1:clust_num) {
    temp_clust_P <- temp_P[grep(paste0("Syn", clust, "$"), rownames(temp_P)), ]
    temp_clust_M <- temp_M[grep(paste0("Syn", clust, "$"), rownames(temp_M)), ]

    if (all(class(temp_clust_P) != "numeric")) {
      mean_P[clust, ] <- colSums(temp_clust_P)
    } else {
      mean_P[clust, ] <- temp_clust_P
    }

    if (all(class(temp_clust_M) != "numeric")) {
      mean_M[clust, ] <- colSums(temp_clust_M)
    } else {
      mean_M[clust, ] <- temp_clust_M
    }

    mean_P[clust, ] <- mean_P[clust, ] - min(mean_P[clust, ])
    mean_M[clust, ] <- mean_M[clust, ] - min(mean_M[clust, ])
    mean_P[clust, ] <- mean_P[clust, ] / max(mean_P[clust, ])
    mean_M[clust, ] <- mean_M[clust, ] / max(mean_M[clust, ])
  }

  # Create ordering rule
  order_rule <- data.frame(
    old = c(1:clust_num),
    new = order(apply(mean_P, 1, CoA))
  )

  # Apply new order to mean curves
  mean_P <- mean_P[order_rule$new, ]
  mean_M <- mean_M[order_rule$new, ]
  rownames(mean_P) <- paste0("Syn", 1:nrow(mean_P))
  rownames(mean_M) <- paste0("Syn", 1:nrow(mean_M))
  colnames(mean_M) <- colnames(temp_M)

  # Apply new order to all
  temp_P <- orders$clusters_P
  temp_M <- orders$clusters_M
  orders$clusters_P <- c(order_rule$old, temp_P)[match(temp_P, c(order_rule$new, temp_P))]
  orders$clusters_M <- c(order_rule$old, temp_M)[match(temp_M, c(order_rule$new, temp_M))]

  if (inspect) {
    # Plot classified syns
    # Find plot size
    dev_size <- grDevices::dev.size(units = "in")
    # Margins are specified in inches following the order:
    # bottom, left, top, right
    # Reduction factor of margins to account for screens at different resolutions
    red_factor <- 25
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(
      mfrow = c(clust_num, 2),
      mai = c(
        dev_size[2] / red_factor,
        dev_size[1] / red_factor,
        dev_size[2] / red_factor,
        dev_size[1] / red_factor
      )
    )

    for (syn in 1:clust_num) {
      # Plot motor modules
      graphics::barplot(mean_M[syn, ])
      # Plot motor primitives
      plot(
        x = c(1:ncol(mean_P)), y = mean_P[syn, ],
        ty = "l", main = paste0("Synergy ", syn),
        xlab = "", ylab = "",
        xaxt = "n", yaxt = "n", lwd = 2
      )
    }

    qq <- 1
    while (!is.na(qq)) {
      message("\nDo you want to change order of classified synergies (type 'y' for 'yes' or 'n' for 'no')?")
      qq <- readline()
      # Break if user decides
      if (qq == "y" || qq == "yes" || qq == "n" || qq == "no") break
    }

    if (qq == "y" || qq == "yes") {
      rep <- "n"
      while (rep == "n" || rep == "no") {

        # Prompt for decision
        message("Press Esc to stop (classification will be interrupted)")
        orders_new <- numeric()
        for (cc in 1:clust_num) {
          pp <- 0.2
          while (pp < 1) {
            pp <- readline(paste0("Syn num to be associated with the curve ", cc, ": "))

            if (grepl("^$", pp)) {
              pp <- -1
            } else if (grepl("\\D", pp) && !grepl("^s$", pp)) {
              pp <- -1
            } else if (grepl("^s$", pp)) {
              pp <- 1000
            } else if (as.numeric(pp) > clust_num) {
              pp <- -1
            }
          }
          orders_new[cc] <- pp
        }
        orders_new <- as.numeric(orders_new)
        orders_new <- sort.int(orders_new, index.return = T)$ix

        # Re-create ordering rule
        order_rule <- data.frame(
          old = c(1:clust_num),
          new = orders_new
        )

        # Make new plots for checking
        mean_P_temp <- mean_P[order_rule$new, ]
        mean_M_temp <- mean_M[order_rule$new, ]
        rownames(mean_P_temp) <- paste0("Syn", 1:nrow(mean_P_temp))
        rownames(mean_M_temp) <- paste0("Syn", 1:nrow(mean_M_temp))

        # Re-plot classified synergies
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        graphics::par(
          mfrow = c(clust_num, 2),
          mai = c(
            dev_size[2] / red_factor,
            dev_size[1] / red_factor,
            dev_size[2] / red_factor,
            dev_size[1] / red_factor
          )
        )

        for (syn in 1:clust_num) {
          # Plot motor modules
          graphics::barplot(mean_M_temp[syn, ])
          # Plot motor primitives
          plot(
            x = c(1:ncol(mean_P_temp)), y = mean_P_temp[syn, ],
            ty = "l", main = paste0("Synergy ", syn),
            xlab = "", ylab = "",
            xaxt = "n", yaxt = "n", lwd = 2
          )
        }
        Sys.sleep(2)

        message("\nAre you sure (see new plot, type 'y' for 'yes' or 'n' for 'no')?")
        rep <- readline()

        if (rep == "n" || rep == "no") {
          for (syn in 1:clust_num) {
            # Plot motor modules
            graphics::barplot(mean_M[syn, ])
            # Plot motor primitives
            plot(
              x = c(1:ncol(mean_P)), y = mean_P[syn, ],
              ty = "l", main = paste0("Synergy ", syn),
              xlab = "", ylab = "",
              xaxt = "n", yaxt = "n", lwd = 2
            )
          }
        }
      }
      # Apply new order to mean curves
      mean_P <- mean_P[order_rule$new, ]
      mean_M <- mean_M[order_rule$new, ]
      rownames(mean_P) <- paste0("Syn", 1:nrow(mean_P))
      rownames(mean_M) <- paste0("Syn", 1:nrow(mean_M))

      # Apply new order to all
      temp_P <- orders$clusters_P
      temp_M <- orders$clusters_M
      orders$clusters_P <- c(order_rule$old, temp_P)[match(temp_P, c(order_rule$new, temp_P))]
      orders$clusters_M <- c(order_rule$old, temp_M)[match(temp_M, c(order_rule$new, temp_M))]
    }
  }

  # Remove double classifications, if present
  trials <- unique(gsub("_Syn.+", "", rownames(orders)))

  for (tt in trials) {
    trial <- orders[grep(paste0(tt, "_"), rownames(orders)), ]
    trial$clusters_M <- NULL

    # Find duplicates
    dupl <- which(duplicated(trial$clusters_P))

    if (length(dupl) > 0) {
      for (syn in c(1:clust_num)) {
        dupl <- grep(paste0("^", syn, "$"), trial$clusters_P)

        if (length(dupl) <= 1) {
          next
        } else {
          R2 <- numeric()
          P2 <- as.numeric(mean_P[syn, ])
          for (dd in c(1:length(dupl))) {
            trial_syn <- rownames(trial)[dupl[dd]]
            # Calculate R2 between each duplicated primitive and the mean
            P1 <- as.numeric(data_P[grep(trial_syn, rownames(data_P)), ])
            RSS <- sum((P1 - P2)^2)
            SST <- sum((P1 - mean(P1))^2)
            R2[dd] <- 1 - (RSS / SST)
          }
          dupl <- dupl[-which.max(R2)]
          trial$clusters_P[dupl] <- "combined"
        }
      }
    }

    if (tt == trials[1]) {
      orders_new <- trial
    } else {
      orders_new <- rbind(orders_new, trial)
    }
  }

  ggfinal <- ggplot2::ggplot(
    data = orders_new,
    ggplot2::aes(
      x = FWHM_P, y = CoA_P,
      colour = factor(paste0("Syn ", clusters_P))
    )
  ) +
    ggplot2::geom_point(size = 4, alpha = 0.5) +
    ggplot2::xlim(0, points) +
    ggplot2::ylim(0, points) +
    ggplot2::labs(title = "Final clustering") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank()
    )

  # Plot on active graphic device if needed
  if (show_plot) {
    # Prepare graphic device
    graphics::plot.new()
    # Plot
    print(ggfinal)
  }

  trial <- gsub("_Syn[0-9]*$", "", rownames(orders_new))
  new <- paste0(trial, "_Syn", orders_new$clusters_P)

  orders_new <- data.frame(
    old = rownames(orders_new),
    new,
    syn_classified = gsub(".*_Syn", "", new),
    trial
  )

  combined <- length(grep("Syncombined", orders_new$new))
  total <- nrow(orders_new)

  if (interactive()) {
    message(
      "\n  Total synergies: ", total,
      "\n       Recognised: ", total - combined,
      "\n         Combined: ", combined, " (~", round(combined / total * 100, 0), "%)",
      "\n         Clusters: ", clust_num, "\n"
    )

    syn_perc <- numeric()
    for (ss in 1:clust_num) {
      syn_perc[ss] <- round(length(grep(
        paste0("^", ss, "$"),
        orders_new$syn_classified
      )) / length(trials) * 100, 0)
      message("             Syn", ss, ": ", syn_perc[ss], "%")
    }
  }

  # Rename synergies in the correct order and save
  SYNS_classified <- x

  # Find unique trial names
  trials <- which(!duplicated(orders_new$trial))

  for (uu in seq_along(trials)) {

    # Read the classification
    if (uu < length(trials)) {
      classification <- orders_new$syn_classified[trials[uu]:(trials[uu + 1] - 1)]
    } else {
      classification <- orders_new$syn_classified[trials[uu]:nrow(orders_new)]
    }

    trial <- orders_new$trial[trials[uu]]

    colnames(SYNS_classified[[trial]]$P) <- c("time", paste0("Syn", classification))
    colnames(SYNS_classified[[trial]]$M) <- paste0("Syn", classification)
    SYNS_classified[[trial]]$classification <- "k-means"
  }

  return(SYNS_classified)
}
