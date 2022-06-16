#' Short-term maximum Lyapunov exponents
#'
#' @param synergies A `musclesyneRgies` object
#' @param mean_period To locate the nearest neighbour of each point on the state space
#' trajectory
#' @param future_pts To limit the number of points "in the future" that are being searched
#' @param norm Type of normalisation ("u" for minimum subtraction and normalisation to the maximum,
#' "z" for subtracting the mean and then divide by the standard deviation)
#' @param pts  Minimum number of points needed to linearly approximate the first part
#' of the divergence curve
#' @param R2_threshold Threshold for calculating the slope of the divergence curve
#'
#' @details
#' The mean period is intended to exclude temporally close points. In gait, values are usually
#' plus/minus half gait cycle. Future points usually correspond in gait to one to two gait cycles.
#' Please consider that a sufficient amount of cycles in order to compute meaningful sMLE.
#' For locomotor primitives, 30 gait cycles have been shown to be sensitive to perturbations
#' (Santuz et al. 2020). However, in the more classical and widespread use on kinematic data,
#' more are usually needed (Kang and Dingwell, 2006).
#'
#' @return
#' A list with elements:\cr
#' - `divergences` containing the average logarithmic divergence curve\cr
#' - `sMLE` the short-term Maximum Lyapunov exponent\cr
#' - `R2` the goodness of fit of the most linear part of the divergence curve
#'
#' @export
#'
#' @references
#' Rosenstein, M.T., Collins, J.J., and De Luca, C.J. (1993).
#' A practical method for calculating largest Lyapunov exponents from small data sets.
#' Phys. D 65, 117–134.\cr
#'
#' Santuz A, Brüll L, Ekizos A, Schroll A, Eckardt N, Kibele A, et al.
#' Neuromotor Dynamics of Human Locomotion in Challenging Settings.
#' iScience. 2020;23: 100796.\cr
#'
#' Kang H.G., and Dingwell J.B. (2006).
#' Intra-session reliability of local dynamic stability of walking.
#' Gait Posture. 24(3) 386-390.
#'
#' @examples
#' # Load some primitives
#' data("primitives")
#' # Calculate sMLE of motor primitives in the muscle synergy space
#' short_term_MLE <- sMLE(primitives,
#'   mean_period = 80,
#'   future_pts = 200,
#'   norm = "z",
#'   pts = 30
#' )
sMLE <- function(synergies, mean_period, future_pts, norm, pts, R2_threshold = 0.9) {
  if (!inherits(synergies, "musclesyneRgies")) {
    stop("Object is not of class musclesynergies, please create objects in the right format with \"synsNMF\"")
  } else {
    # Get motor primitives
    # To include cases with only one synergy and keep the format, time series is forced to be a matrix
    # Matrices are faster than data frames (in this case around twice as fast)
    P <- as.matrix(synergies$P[, -1])
  }

  # Normalise time series
  if (norm == "u") {
    # Subtract the minimum
    P <- apply(P, 2, function(x) x - min(x))
    # Amplitude normalisation to the maximum of the trial
    P <- apply(P, 2, function(x) x / max(x))
  } else if (norm == "z") {
    # Subtract the mean and divide by the standard deviation
    P <- apply(P, 2, function(x) (x - mean(x)) / stats::sd(x))
  }

  # Nearest neighbour location
  # This method is around 1000 times faster than for loops with 3D data of around 6000 rows
  upper_limit <- nrow(P) - future_pts
  # For primitives with cycles of 200 data points, the maximum number of NN to search (k) can be 50
  # but ideally not less (in case of 200-point cycles, 100 can be a good choice)
  temp <- FNN::get.knn(P[1:upper_limit, ], k = mean_period)$nn.index
  neighbours <- numeric()

  for (rr in seq_len(nrow(temp))) {
    test_sup <- rr + mean_period
    test_inf <- rr - mean_period
    if (test_inf <= 0) test_inf <- 1
    temp_sup <- which(temp[rr, ] > test_sup)[1]
    temp_inf <- which(temp[rr, ] < test_inf)[1]

    neighbours[rr] <- temp[rr, min(temp_sup, temp_inf, na.rm = TRUE)]
  }

  # Calculate distances (and their divergence) between neighbouring trajectories
  dist_logs <- matrix(NA, length(neighbours), future_pts)
  for (tt in seq_len(length(neighbours))) {
    # Identify the two (initially) neighbouring trajectories
    traj2 <- as.matrix(P[neighbours[tt]:(neighbours[tt] + future_pts - 1), ])
    traj1 <- as.matrix(P[tt:(tt + future_pts - 1), ])

    # This is around 100 times faster than a for loop
    # See how it works here: https://goo.gl/iDGgP3
    dists <- proxy::dist(traj1, traj2, pairwise = TRUE)

    # Calculate the natural logarithm of the distances
    dist_logs[tt, ] <- log(dists)
  }
  dist_logs[dist_logs == -Inf] <- NA
  dist_logs_av <- apply(dist_logs, 2, function(x) mean(x, na.rm = TRUE))

  # Calculate short-term maximum Lyapunov exponent (sMLE)
  # Linear part
  x <- 1:pts
  y <- dist_logs_av[x]

  if (sum(y) == -Inf) {
    temp <- cbind(x, y)
    temp <- temp[-grep("-Inf", temp[, 2]), ]
    x <- temp[, 1]
    y <- temp[, 2]
  }
  # Slope (sMLE)
  R2 <- 0
  while (R2 < R2_threshold) {
    x <- x[-length(x)]
    y <- y[-length(y)]
    R2 <- summary(stats::lm(y ~ x))$r.squared
  }
  R2 <- summary(stats::lm(y ~ x))$r.squared

  m <- as.numeric(stats::coef(stats::lm(y ~ x))["x"])

  return(list(
    divergences = dist_logs_av,
    sMLE = m,
    R2 = R2
  ))
}
