#' Muscle synergy classification with NMF
#'
#' @param x A list of `musclesyneRgies` objects
#' @param interactive Logical, ask for interactive re-ordering or go fully automated?
#'
#' @details
#' This function must be applied to a list with a sufficient amount of trials, otherwise the
#' classification will not work. Typically, at least 10 trials for the same condition are needed
#' for satisfactory classification.
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
#' - `classification` classification type (`NMF`)
#'
#' @export
#'
#' @examples
#' # Load some data
#' data(SYNS)
#' # Classify synergies
#' SYNS_classified <- classify_NMF(SYNS,
#'   interactive = FALSE
#' )
classify_NMF <- function(x,
                         interactive = TRUE) {

  # Define NMF function
  NMFn <- function(V) {
    R2_target <- 0.01 # Convergence criterion (percent of the R2 value)
    R2_cross <- numeric() # R2 values for cross validation and syn number assessment
    M_list <- list() # To save factorisation M matrices (synergies)
    P_list <- list() # To save factorisation P matrices (primitives)

    # Original matrix
    V <- as.matrix(V)
    V[V < 0] <- 0 # Set negative values to zero
    temp <- V
    temp[temp == 0] <- Inf
    V[V == 0] <- min(temp, na.rm = T) # Set the zeros to the smallest non-zero entry in V

    m <- nrow(V) # Number of primitives in the data-set
    n <- ncol(V) # Number of time points

    # Determine the maximum number of synergies by searching for the maximum rank
    temp <- as.numeric(gsub(".*\\_Syn", "", rownames(V)))
    # Add one because interpolation must happen with at least two points
    max_syns <- max(temp) + 1

    for (r in 1:max_syns) { # Run NMF with different initial conditions
      R2_choice <- numeric() # Collect the R2 values for each syn and choose the max

      # Preallocate to then choose those with highest R2
      M_temp <- list()
      P_temp <- list()

      for (j in 1:5) { # Run NMF 5 times for each syn and choose best run
        # To save error values
        R2 <- numeric() # 1st cost function (R squared)
        SST <- numeric() # Total sum of squares
        RSS <- numeric() # Residual sum of squares or min reconstruction error

        # Initialise iterations and define max number of iterations
        iter <- 1
        max_iter <- 1000
        # Initialise the two factorisation matrices with random values
        # (uniform distribution)
        P <- matrix(stats::runif(r * n, min = min(V), max = max(V)), nrow = r, ncol = n)
        M <- matrix(stats::runif(m * r, min = min(V), max = max(V)), nrow = m, ncol = r)

        # Iteration zero
        P <- P * crossprod(M, V) / crossprod((crossprod(M, M)), P)
        M <- M * tcrossprod(V, P) / tcrossprod(M, tcrossprod(P, P))
        Vr <- M %*% P # Reconstructed matrix
        RSS <- sum((V - Vr)^2)
        SST <- sum((V - mean(V))^2)
        R2[iter] <- 1 - (RSS / SST)

        # l2-norm normalisation which eliminates trivial scale indeterminacies
        for (kk in 1:r) {
          norm <- sqrt(sum(M[, kk]^2))
          M[, kk] <- M[, kk] / norm
          P[kk, ] <- P[kk, ] * norm
        }

        # Start iterations for NMF convergence
        for (iter in iter:max_iter) {
          P <- P * crossprod(M, V) / crossprod((crossprod(M, M)), P)
          M <- M * tcrossprod(V, P) / tcrossprod(M, tcrossprod(P, P))
          Vr <- M %*% P
          RSS <- sum((V - Vr)^2)
          SST <- sum((V - mean(V))^2)
          R2[iter] <- 1 - (RSS / SST)

          # l2-norm normalisation
          for (kk in 1:r) {
            norm <- sqrt(sum(M[, kk]^2))
            M[, kk] <- M[, kk] / norm
            P[kk, ] <- P[kk, ] * norm
          }

          # Check if the increase of R2 in the last 20 iterations is less than the target
          if (iter > 20) {
            R2_diff <- R2[iter] - R2[iter - 20]
            if (R2_diff < R2[iter] * R2_target / 100) {
              break
            }
          }
        }
        R2_choice[j] <- R2[iter]

        M_temp[[j]] <- M
        P_temp[[j]] <- P
      }

      choice <- which.max(R2_choice)

      R2_cross[r] <- R2_choice[choice]
      M_list[[r]] <- M_temp[[choice]]
      P_list[[r]] <- P_temp[[choice]]
    }

    # Choose the minimum number of principal shapes using the R2 criterion
    MSE <- 100 # Initialise the Mean Squared Error (MSE)
    iter <- 0 # Initialise iterations
    while (MSE > 1e-04) {
      iter <- iter + 1
      if (iter == r - 1) {
        break
      }
      R2_interp <- data.frame(
        synergies = c(1:(r - iter + 1)),
        R2_values = R2_cross[iter:r]
      )

      lin <- stats::lm(R2_values ~ synergies, R2_interp)$fitted.values
      MSE <- sum((lin - R2_interp$R2_values)^2) / nrow(R2_interp)
    }
    syns_R2 <- iter

    return(list(
      M = M_list[[syns_R2]],
      P = P_list[[syns_R2]]
    ))
  }

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

  message("\nCalculating mean gait cycles...")

  P <- pbapply::pblapply(P, function(y) {
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
  message("...done!")

  message("\nPutting primitives into a single data frame...")
  data_P <- plyr::ldply(P, function(y) data.frame(y), .progress = plyr::progress_text(char = "+"))
  message("...done!")

  message("\nPutting modules into a single data frame...")
  data_M <- plyr::ldply(M, function(y) t(data.frame(y)), .progress = plyr::progress_text(char = "+"))
  message("...done!")

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

  # Apply NMF
  data_NMF <- lapply(list(data_P), NMFn)[[1]]

  data_all <- data_P
  order_list <- list()

  data_NMF_P <- data_NMF$P
  data_NMF_M <- data_NMF$M
  syns_num_n <- ncol(data_NMF_M)

  # Order using CoA
  orders <- order(apply(data_NMF_P, 1, CoA))

  data_NMF_P <- data_NMF_P[orders, ]
  data_NMF_M <- data_NMF_M[, orders]

  # Normalise to 1
  data_NMF_P <- data.frame(t(apply(data_NMF_P, 1, function(y) y / max(y))))
  data_NMF_M <- data.frame(apply(data_NMF_M, 2, function(y) y / max(y)))

  rownames(data_NMF_P) <- paste0("Syn", 1:nrow(data_NMF_P))
  colnames(data_NMF_M) <- paste0("Syn", 1:ncol(data_NMF_M))

  if (isTRUE(interactive)) {
    # Plot classified syns
    # Find plot size
    dev_size <- grDevices::dev.size(units = "in")
    # Margins are specified in inches following the order:
    # bottom, left, top, right
    # Reduction factor of margins to account for screens at different resolutions
    red_factor <- 35
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(
      mfrow = c(syns_num_n, 2),
      mai = c(
        dev_size[2] / red_factor,
        dev_size[1] / red_factor,
        dev_size[2] / red_factor,
        dev_size[1] / red_factor
      )
    )

    for (syn in 1:syns_num_n) {
      plot(
        x = c(1:ncol(data_NMF_P)), y = data_NMF_P[syn, ],
        ty = "l", main = paste0("Synergy ", syn),
        xlab = "", ylab = "",
        xaxt = "n", yaxt = "n", lwd = 2
      )
      graphics::barplot(sort(data_NMF_M[, syn], decreasing = T))
      graphics::abline(h = seq(0.2, 0.8, 0.1), col = 2)
      tot <- length(data_NMF_M[, syn])
      graphics::abline(v = tot * seq(0, 1, 0.25), col = 2)
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
        message("Press Esc to stop (order will not be changed)")
        orders_new <- numeric()
        for (cc in 1:nrow(data_NMF_P)) {
          pp <- 0.2
          while (pp < 1) {
            pp <- readline(paste0("Syn num to be associated with the curve ", cc, ": "))

            if (grepl("^$", pp)) {
              pp <- -1
            } else if (grepl("\\D", pp) && !grepl("^s$", pp)) {
              pp <- -1
            } else if (grepl("^s$", pp)) {
              pp <- 1000
            } else if (as.numeric(pp) > syns_num_n) {
              pp <- -1
            }
          }
          orders_new[cc] <- pp
        }

        orders_new <- as.numeric(orders_new)

        orders <- sort.int(orders_new, index.return = T)$ix

        # Make new plots for checking
        data_NMF_P_temp <- data_NMF_P[orders, ]
        data_NMF_M_temp <- data_NMF_M[, orders]

        # Normalise to 1
        data_NMF_P_temp <- data.frame(t(apply(data_NMF_P_temp, 1, function(x) x / max(x))))
        data_NMF_M_temp <- data.frame(apply(data_NMF_M_temp, 2, function(x) x / max(x)))

        rownames(data_NMF_P_temp) <- paste0("Syn", 1:nrow(data_NMF_P_temp))
        colnames(data_NMF_M_temp) <- paste0("Syn", 1:ncol(data_NMF_M_temp))

        # Re-plot classified syns
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(oldpar))
        graphics::par(
          mfrow = c(syns_num_n, 2),
          mai = c(
            dev_size[2] / red_factor,
            dev_size[1] / red_factor,
            dev_size[2] / red_factor,
            dev_size[1] / red_factor
          )
        )

        for (syn in 1:syns_num_n) {
          plot(
            x = c(1:ncol(data_NMF_P_temp)), y = data_NMF_P_temp[syn, ],
            ty = "l", main = paste0("Synergy ", syn),
            xlab = "", ylab = "",
            xaxt = "n", yaxt = "n", lwd = 2
          )
          graphics::barplot(sort(data_NMF_M_temp[, syn], decreasing = T))
          graphics::abline(h = seq(0.2, 0.8, 0.1), col = 2)
          tot <- length(data_NMF_M_temp[, syn])
          graphics::abline(v = tot * seq(0, 1, 0.25), col = 2)
        }

        Sys.sleep(2)

        message("\nAre you sure (see new plot, type 'y' for 'yes' or 'n' for 'no')?")
        rep <- readline()

        if (rep == "n" || rep == "no") {
          for (syn in 1:syns_num_n) {
            plot(
              x = c(1:ncol(data_NMF_P)), y = data_NMF_P[syn, ],
              ty = "l", main = paste0("Synergy ", syn, ),
              xlab = "", ylab = "",
              xaxt = "n", yaxt = "n", lwd = 2
            )
            graphics::barplot(sort(data_NMF_M[, syn], decreasing = T))
            graphics::abline(h = seq(0.2, 0.8, 0.1), col = 2)
            tot <- length(data_NMF_M[, syn])
            graphics::abline(v = tot * seq(0, 1, 0.25), col = 2)
          }
        }
      }
      # Apply new orders
      data_NMF_P <- data_NMF_P[orders, ]
      data_NMF_M <- data_NMF_M[, orders]

      # Normalise to 1
      data_NMF_P <- data.frame(t(apply(data_NMF_P, 1, function(x) x / max(x))))
      data_NMF_M <- data.frame(apply(data_NMF_M, 2, function(x) x / max(x)))

      rownames(data_NMF_P) <- paste0("Syn", 1:nrow(data_NMF_P))
      colnames(data_NMF_M) <- paste0("Syn", 1:ncol(data_NMF_M))
    }
  }



  # Now search for syns having module bigger than "M_threshold"
  # If a syn has more than one module, choose the one with the highest value
  # Then compare the primitive to the relevant one and assess similarity
  # If similarity is lower than "R2_threshold", classify as combined, otherwise keep
  temp <- data_NMF_M

  # Determine the threshold for M and R2
  # Calculate the M threshold
  M_threshold <- mean(colMeans(temp, na.rm = T), na.rm = T)

  temp[temp < M_threshold] <- NA

  quality <- numeric()

  for (tt in 1:nrow(temp)) {
    if (sum(temp[tt, ], na.rm = T) == 0) {
      quality[tt] <- NA
      next
    } else if (sum(is.na(temp[tt, ])) >= 1 || sum(temp[tt, ], na.rm = T) != 0) {
      # Find position of maximum
      choice <- which(temp[tt, ] == max(temp[tt, ], na.rm = T))
      # Discard the others
      temp[tt, -choice] <- NA
      # Calculate R2 between curve and primitive
      P1 <- as.numeric(data_NMF_P[choice, ])
      P2 <- as.numeric(data_P[tt, ])
      RSS <- sum((P1 - P2)^2)
      SST <- sum((P1 - mean(P1))^2)
      R2 <- 1 - (RSS / SST)

      quality[tt] <- R2
    }
  }

  # Calculate the R2 threshold
  R2_threshold <- mean(quality, na.rm = T)
  if (sign(R2_threshold) == 1) {
    R2_threshold <- R2_threshold / 4
  } else if (sign(R2_threshold) == -1) {
    R2_threshold <- R2_threshold * 4
  }

  quality[quality <= R2_threshold] <- NA

  classification <- numeric()
  weight <- numeric()

  for (tt in 1:nrow(temp)) {
    if (sum(temp[tt, ], na.rm = T) == 0) {
      classification[tt] <- NA
      weight[tt] <- NA
      next
    } else if (sum(is.na(temp[tt, ])) >= 1 || sum(temp[tt, ], na.rm = T) != 0) {
      # Find position of maximum
      choice <- which(temp[tt, ] == max(temp[tt, ], na.rm = T))
      # Discard the others
      temp[tt, -choice] <- NA
      # Discard if R2 between curve and primitive is lower than R2_threshold
      R2 <- quality[tt]

      if (is.na(R2)) {
        classification[tt] <- NA
        weight[tt] <- NA
      } else {
        classification[tt] <- choice
        weight[tt] <- temp[tt, choice]
      }
    }
  }

  trial <- gsub("_Syn.*", "", rownames(temp))
  syn <- as.numeric(gsub(".*_Syn", "", rownames(temp)))

  ordered <- data.frame(
    trial = trial,
    syn_original = syn,
    syn_classified = classification,
    weight = weight,
    quality = quality
  )

  syns_num <- max(ordered$syn_classified, na.rm = T)

  ordered$syn_classified[which(is.na(ordered$syn_classified))] <- "combined"

  # Remove double classifications, if present
  # Find unique trial names
  trials <- which(!duplicated(ordered$trial))
  for (uu in seq_along(trials)) {

    # Read the classification
    if (uu < length(trials)) {
      temp1 <- ordered[trials[uu]:(trials[uu + 1] - 1), ]
    } else {
      temp1 <- ordered[trials[uu]:nrow(ordered), ]
    }

    # Find duplicates
    temp2 <- which(duplicated(temp1$syn_classified))

    if (length(temp2) == 0) {
      next
    } else {
      for (syn in c((1:syns_num), "combined")) {
        temp2 <- grep(paste0("^", syn, "$"), temp1$syn_classified)

        if (length(temp2) <= 1) {
          next
        } else {
          # Remove lower quality trials
          temp3 <- as.numeric(temp1$quality[temp2])
          temp2 <- temp2[-which(temp3 == max(temp3))]
          temp1$syn_classified[temp2] <- NA
        }
      }
    }
    # Save the new data
    if (uu < length(trials)) {
      ordered[trials[uu]:(trials[uu + 1] - 1), ] <- temp1
    } else {
      ordered[trials[uu]:nrow(ordered), ] <- temp1
    }
  }

  ordered$syn_classified[which(is.na(ordered$syn_classified))] <- "combined"

  ordered <- data.frame(
    old = paste0(ordered$trial, "_Syn", ordered$syn_original),
    new = paste0(ordered$trial, "_Syn", ordered$syn_classified),
    ordered
  )

  ordered[] <- lapply(ordered, as.character)

  combined <- length(grep("combined", ordered$syn_classified))
  total <- nrow(ordered)

  message(
    "\n  Total synergies: ", total,
    "\n       Recognised: ", total - combined,
    "\n         Combined: ", combined, " (~", round(combined / total * 100, 0), "%)",
    "\n     R2 threshold: ", round(R2_threshold, 2),
    "\n Module threshold: ", round(M_threshold, 2), "\n"
  )

  syn_perc <- numeric()
  for (ss in 1:syns_num) {
    syn_perc[ss] <- round(length(grep(
      paste0("^", ss, "$"),
      ordered$syn_classified
    )) / length(trials) * 100, 0)
    message("             Syn", ss, ": ", syn_perc[ss], "%")
  }

  # Rename synergies in the correct order and save
  SYNS_classified <- x

  # Find unique trial names
  trials <- which(!duplicated(ordered$trial))

  for (uu in seq_along(trials)) {

    # Read the classification
    if (uu < length(trials)) {
      classification <- ordered$syn_classified[trials[uu]:(trials[uu + 1] - 1)]
    } else {
      classification <- ordered$syn_classified[trials[uu]:nrow(ordered)]
    }

    trial <- ordered$trial[trials[uu]]

    colnames(SYNS_classified[[trial]]$P) <- c("time", paste0("Syn", classification))
    colnames(SYNS_classified[[trial]]$M) <- paste0("Syn", classification)
    SYNS_classified[[trial]]$classification <- "NMF"
  }

  return(SYNS_classified)
}
