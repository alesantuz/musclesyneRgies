#' Non-negative matrix factorisation
#'
#' @param V EMG data frame to be reconstructed, usually filtered and time-normalised
#' @param R2_target Threshold to stop iterations for a certain factorisation rank
#' @param runs Number of repetitions for each rank to avoid local minima
#' @param max_iter Maximum number of iterations allowed for each rank
#' @param last_iter How many of the last iterations should be checked before stopping?
#' @param MSE_min Threshold on the mean squared error to choose the factorisation rank or
#' minimum number of synergies
#' @param fixed_syns To impose the factorisation rank or number of synergies
#'
#' @details
#' The first column of `V` must always contain time information.
#'
#' @return
#' Object of class `musclesyneRgies` with elements:\cr
#' - `syns` factorisation rank or minimum number of synergies\cr
#' - `M` muscle weights (time-invariant coefficients)\cr
#' - `P` activation patterns (time-dependent coefficients)\cr
#' - `V` original data\cr
#' - `Vr` reconstructed data\cr
#' - `iterations` number of iterations to convergence\cr
#' - `R2` quality of reconstruction (coefficient of determination)
#' - `rank_type` was the rank `fixed` or `variable`?\cr
#' - `classification` classification type (e.g., `none`, `k-means`, `NMF`, etc.)
#'
#' @export
#'
#' @references
#' Lee, D. D. & Seung, H. S.
#' Learning the parts of objects by non-negative matrix factorization.
#' Nature 401, 788-91 (1999).\cr
#'
#' Santuz, A., Ekizos, A., Janshen, L., Baltzopoulos, V. & Arampatzis, A.
#' On the Methodological Implications of Extracting Muscle Synergies from Human Locomotion.
#' Int. J. Neural Syst. 27, 1750007 (2017).\cr
#'
#' Févotte, C., Idier, J.
#' Algorithms for Nonnegative Matrix Factorization with the Beta-Divergence
#' Neural Computation 23, 9 (2011).
#'
#' @examples
#' # Note that for bigger data sets one might want to run computation in parallel
#' # Load some data
#' data(FILT_EMG)
#' # Extract synergies (careful, rank is imposed here!)
#' SYNS <- lapply(FILT_EMG, synsNMF, fixed_syns = 4)
synsNMF <- function(V,
                    R2_target = 0.01,
                    runs = 5,
                    max_iter = 1000,
                    last_iter = 20,
                    MSE_min = 1e-04,
                    fixed_syns = NA) {
  if (!inherits(V, "data.frame")) {
    stop("Object is not a data frame")
  }

  # Put aside time information
  time <- V$time

  R2_cross <- numeric() # R2 values for cross validation and syn number assessment
  M_list <- list() # To save factorisation M matrices (muscle weights)
  P_list <- list() # To save factorisation P matrices (activation patterns)
  Vr_list <- list() # To save factorisation Vr matrices (reconstructed signals)
  iterations <- numeric() # To save the iterations number

  # Remove time column and transpose for upcoming NMF
  V <- subset(V, select = -time) |>
    t() |>
    as.matrix()

  # Replace values <= 0 with the smallest non-zero value
  V[V <= 0] <- min(V[V > 0], na.rm = TRUE)

  m <- nrow(V) # Number of muscles
  n <- ncol(V) # Number of time points

  if (is.na(fixed_syns)) {
    min_syns <- 1
    max_syns <- m - round(m / 4, 0) # Max number of syns
    rank_type <- "variable"
  } else if (is.numeric(fixed_syns)) {
    min_syns <- max_syns <- fixed_syns
    rank_type <- "fixed"
  }

  syn_index <- 0
  for (r in min_syns:max_syns) { # Run NMF with different initial conditions
    syn_index <- syn_index + 1
    R2_choice <- numeric() # Collect the R2 values for each syn and choose the max

    # Preallocate to then choose those with highest R2
    M_temp <- list()
    P_temp <- list()
    Vr_temp <- list()
    iters <- numeric()

    for (run in 1:runs) { # Run NMF multiple times for each syn and choose best run
      # Initialise the two factorisation matrices with random values (uniform distribution)
      P <- matrix(stats::runif(r * n, min = min(V), max = max(V)), nrow = r, ncol = n)
      M <- matrix(stats::runif(m * r, min = min(V), max = max(V)), nrow = m, ncol = r)

      # Iteration "zero" and first updates
      MV <- crossprod(M, V)
      MM <- crossprod(M, M)
      P <- P * MV / MM %*% P
      VP <- tcrossprod(V, P)
      PP <- tcrossprod(P, P)
      M <- M * VP / tcrossprod(M, PP)
      # Reconstruction
      Vr <- M %*% P
      # Reconstruction quality (coefficient of determination)
      R2 <- 1 - sum((V - Vr)^2) / sum((V - mean(V))^2)

      # l2-norm normalisation eliminates trivial scale indeterminacies
      # See Févotte, C., Idier, J. (2011)
      # Calculate l2-norm of the columns of M and normalise M and P
      l2_norms <- sqrt(colSums(M^2))
      M <- M / matrix(l2_norms, nrow = nrow(M), ncol = ncol(M), byrow = TRUE)
      P <- P * l2_norms

      # Start iterations for NMF convergence
      iter <- 1
      while (iter < max_iter) {
        iter <- iter + 1

        # Updates
        MV <- crossprod(M, V)
        MM <- crossprod(M, M)
        P <- P * MV / MM %*% P
        VP <- tcrossprod(V, P)
        PP <- tcrossprod(P, P)
        M <- M * VP / tcrossprod(M, PP)

        # Reconstruction
        Vr <- M %*% P
        # Reconstruction quality
        R2[iter] <- 1 - sum((V - Vr)^2) / sum((V - mean(V))^2)

        # l2-norm normalisation
        l2_norms <- sqrt(colSums(M^2))
        M <- M / matrix(l2_norms, nrow = nrow(M), ncol = ncol(M), byrow = TRUE)
        P <- P * l2_norms

        # Check if the increase of R2 in the last "last_iter" iterations
        # is less than the target
        if (iter > last_iter &&
            R2[iter] - R2[iter - last_iter] < R2[iter] * R2_target / 100) {
          break
        }
      }
      R2_choice[run] <- R2[iter]
      M_temp[[run]] <- M
      P_temp[[run]] <- P
      Vr_temp[[run]] <- Vr
      iters[run] <- iter
    }

    choice <- which.max(R2_choice)

    R2_cross[syn_index] <- R2_choice[choice]
    M_list[[syn_index]] <- M_temp[[choice]]
    P_list[[syn_index]] <- P_temp[[choice]]
    Vr_list[[syn_index]] <- Vr_temp[[choice]]
    iterations[syn_index] <- iters[choice]
  }

  if (is.na(fixed_syns)) {
    # Choose the minimum number of synergies using the R2 criterion
    MSE <- 100 # Initialise the Mean Squared Error (MSE)
    iter <- 0 # Initialise iterations
    while (MSE > MSE_min) {
      iter <- iter + 1
      if (iter == max_syns - 1) {
        break
      }

      synergies <- 1:(max_syns - iter + 1)
      R2_values <- R2_cross[iter:max_syns]

      lin <- stats::lm(R2_values ~ synergies)$fitted.values
      MSE <- sum((lin - R2_values)^2) / length(R2_values)
    }
    syns_R2 <- iter

    P_choice <- data.frame(time, t(P_list[[syns_R2]]))
    colnames(P_choice) <- c("time", paste0("Syn", seq_len(ncol(P_choice) - 1)))
    rownames(P_choice) <- NULL
    colnames(M_list[[syns_R2]]) <- paste0("Syn", seq_len(ncol(M_list[[syns_R2]])))
    M_choice <- M_list[[syns_R2]]
    Vr_choice <- Vr_list[[syns_R2]]
    iterations <- iterations[syns_R2]
  } else if (is.numeric(fixed_syns)) {
    syns_R2 <- fixed_syns

    P_choice <- data.frame(time, t(P_list[[1]]))
    colnames(P_choice) <- c("time", paste0("Syn", seq_len(ncol(P_choice) - 1)))
    rownames(P_choice) <- NULL
    colnames(M_list[[1]]) <- paste0("Syn", seq_len(ncol(M_list[[1]])))
    M_choice <- M_list[[1]]
    Vr_choice <- Vr_list[[1]]
    iterations <- iterations[1]
  }

  SYNS <- list(
    syns = as.numeric(syns_R2),
    M = M_choice,
    P = P_choice,
    V = V,
    Vr = Vr_choice,
    iterations = iterations,
    R2 = data.frame(
      synergies = min_syns:max_syns,
      R2 = R2_cross
    ),
    classification = "none",
    rank_type = rank_type
  )

  class(SYNS) <- "musclesyneRgies"

  return(SYNS)
}
