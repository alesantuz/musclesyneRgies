#' Hurst exponent
#'
#' @param P A time series (numeric)
#' @param min_win Minimum window length in points
#'
#' @details
#' `Hurst` calculates the Hurst exponent based on the R/S approach as in Hurst (1951).
#' The Hurst exponent is a measure of global complexity and it increases if the “accuracy” of
#' the time series decreases across several cycles (thus the term “global”).
#' The Hurst exponent can vary between 0 and 1. For 0.5 < Hurst exponent < 1, in the long-term high
#' values in the time series (the motor primitive in our case) will be probably followed by other high
#' values and a positive or negative trend is visible (Mandelbrot, 1983; Gneiting & Schlather, 2004).
#' For 0 < Hurst exponent < 0.5, in the long term high values in the series will be probably followed
#' by low values, with a frequent switch between high and low values
#' (Mandelbrot, 1983; Gneiting & Schlather, 2004). Hurst exponent = 0.5 corresponds to a completely
#' random series (Mandelbrot, 1983; Qian & Rasheed, 2004). In other words, values of Hurst
#' exponent approaching 0.5 from both ends indicate more complex (or random) behaviour of the
#' time series (Hurst, 1951).
#' For motor primitives, the minimum window length should be bigger than the period
#' (i.e. the length of each cycle), as reported in Santuz, Akay (2020).
#'
#' @return
#' A list with elements:\cr
#' - `loglog` containing the log-log plot from which the HFD is calculated\cr
#' - `Hurst` containing the Higuchi's fractal dimension of the time series.
#'
#' @export
#'
#' @references
#' Hurst, H. E.
#' Long-term storage capacity of reservoirs.
#' Trans. Am. Soc. Civ. Eng. 116, 770-808 (1951).\cr
#'
#' Mandelbrot B. B.
#' The Fractal Geometry of Nature.
#' W. H. Freeman and Co., New York (1983).\cr
#'
#' Gneiting T .& Schlather M.
#' Stochastic Models That Separate Fractal Dimension and the Hurst Effect.
#' SIAM Rev 46, 269–282 (2004).\cr
#'
#' Qian B. & Rasheed K.
#' Hurst exponent and financial market predictability.
#' In Proceedings of the Second IASTED International Conference on Financial Engineering and
#' Applications, pp. 203–209 (2004).\cr
#'
#' Santuz, A. & Akay, T.
#' Fractal analysis of muscle activity patterns during locomotion: pitfalls and how to avoid them.
#' J. Neurophysiol. 124, 1083-1091 (2020).
#'
#' @examples
#' # Measurements of the annual flow of the river Nile at Aswan
#' flow <- datasets::Nile
#'
#' # Calculate Hurst exponent
#' H <- Hurst(flow)$Hurst
#' message("Hurst exponent: ", round(H, 3))
#'
#' # Thirty-cycle locomotor primitive from Santuz & Akay (2020)
#' data(primitive)
#' H <- Hurst(primitive$signal, min_win = max(primitive$time))$Hurst
#' message("Hurst exponent: ", round(H, 3))
Hurst <- function(P,
                  min_win = 2) {

  # Stop if data is not in the right form
  if (!is.numeric(P) || !is.numeric(min_win)) {
    stop("Please check that time series and min_win are numeric")
  }

  min_win <- max(2, floor(min_win[1]))
  n <- length(P)

  # Data must have an even number of elements
  if (n %% 2 != 0) {
    P <- c(P, (P[n - 1] + P[n]) / 2)
    n <- n + 1
  }

  # Half intervals of indices
  half <- function(ind) {
    sort(c(ind, ind[-length(ind)] + ((diff(ind) + 1) %/% 2)))
  }

  # Define the R/S scale
  rsscale <- function(x) {
    y <- x - mean(x)
    s <- cumsum(y) # cumsum does: c(y[1], y[1]+y[2], y[1]+y[2]+y[3], ...)
    # Cumulated range (difference between min and max of the cumsum)
    R <- diff(range(s))
    # Sample variance
    S <- stats::sd(x)

    # Return the rescaled range
    return(R / S)
  }

  # Set initial values
  X <- n
  Y <- rsscale(P)
  N <- c(0, n %/% 2, n)

  # Compute averaged R/S for halved intervals
  while (min(diff(N)) >= min_win) {
    xl <- c()
    yl <- c()
    for (ii in 2:length(N)) {
      rs <- rsscale(P[(N[ii - 1] + 1):N[ii]])
      xl <- c(xl, N[ii] - N[ii - 1])
      yl <- c(yl, rs)
    }
    X <- c(X, mean(xl))
    Y <- c(Y, mean(yl))
    # Next step
    N <- half(N)
  }

  # Save data for loglog plot
  loglog <- data.frame(
    log_N = log(X),
    log_RS = log(Y)
  )

  # Calculate Hurst exponent as slope of the loglog
  Hurst <- stats::coef(stats::lm(loglog$log_RS ~ loglog$log_N))[2]

  return(list(
    loglog = loglog,
    Hurst = Hurst
  ))
}
