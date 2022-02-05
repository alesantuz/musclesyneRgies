#' Higuchi's fractal dimension
#'
#' @param P A time series (numeric)
#' @param k_max Maximum window length in points
#'
#' @details
#' For motor primitives, only the most linear part of the log-log plot should be used,
#' as reported in Santuz, Akay (2020).
#'
#' @return
#' A list with elements:\cr
#' - `loglog` containing the log-log plot from which the HFD is calculated\cr
#' - `Higuchi` containing the Higuchi's fractal dimension of the time series.
#'
#' @export
#'
#' @references
#' Higuchi, T.
#' Approach to an irregular time series on the basis of the fractal theory.
#' Phys. D Nonlinear Phenom. 31, 277–283 (1988).\cr
#'
#' Santuz, A. & Akay, T.
#' Fractal analysis of muscle activity patterns during locomotion: pitfalls and how to avoid them.
#' J. Neurophysiol. 124, 1083–1091 (2020).
#'
#' @examples
#' # Measurements of the annual flow of the river Nile at Aswan
#' flow <- datasets::Nile
#'
#' # Calculate HFD
#' fractal_dimension <- HFD(flow)$Higuchi
#' message("Higuchi's fractal dimension: ", round(fractal_dimension, 3))
#'
#' # Thirty-cycle locomotor primitive from Santuz & Akay (2020)
#' data(primitive)
#' fractal_dimension <- HFD(primitive$signal)$Higuchi
#' message("Higuchi's fractal dimension: ", round(fractal_dimension, 3))
HFD <- function(P,
                k_max = 10) {

  # Stop if data is not in the right form
  if (!is.numeric(P) && !is.numeric(k_max)) {
    stop("Please check that time series and k_max are numeric")
  }

  k_max <- max(2, floor(k_max[1]))

  N <- length(P)
  L <- numeric()

  k_seq <- seq(2, k_max, 2)

  index <- 0

  for (k in k_seq) {
    index <- index + 1

    # Construct k new time series
    L_m <- numeric()

    for (m in 1:k) {
      # New time series, downsampled from P with time interval k
      P_k <- P[seq(m, length(P), k)]

      # Non-Euclidean length of P_k
      L_m[m] <- 1 / k * (((N - 1) / (k * round((N - 1) / k, 0))) * (sum(abs(diff(P_k)))))
    }
    L[index] <- sum(L_m) / k
  }

  # Save data for loglog plot
  loglog <- data.frame(
    log_k = log(k_seq),
    log_L = log(L)
  )

  # Calculate HFD as slope of the loglog
  Higuchi <- abs(stats::coef(stats::lm(log(L) ~ log(k_seq)))[2])

  return(list(
    loglog = loglog,
    Higuchi = Higuchi
  ))
}
