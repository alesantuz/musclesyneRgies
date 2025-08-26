#' Centre of activity
#'
#' @param x A numeric time series of length n (samples represent bin centres at 1..n).
#' @param tol_rel Relative tolerance (0–1). If the mean resultant length
#' is less than this value, the CoA is considered undefined and `NA` is returned.
#' Default is 1e-6.
#'
#' @return The centre of activity of the time series, calculated with
#' circular statistics: a numeric scalar in (0, n] giving the centre of activity on the 1..n axis.
#'
#' @export
#'
#' @references
#' Martino, G. et al.
#' Locomotor patterns in cerebellar ataxia.
#' J. Neurophysiol. 112, 2810–2821 (2014).
#'
#' @examples
#' # Number of users connected to the Internet through a server every minute
#' ts <- datasets::WWWusage[1:80]
#'
#' # Calculate CoA
#' ts_CoA <- CoA(ts)
#'
#' # Plot
#' plot(ts, ty = "l", xlab = "Time", ylab = "Number of users")
#' graphics::abline(v = ts_CoA, lwd = 2, lty = 2)
CoA <- function(x,
                tol_rel = 1e-6) {
  stopifnot(is.numeric(x), length(x) > 0L)
  n <- length(x)

  # Angles at bin edges: 0, 2π/n, ..., 2π*(n-1)/n
  alpha <- 2 * pi * ((seq_len(n) - 1) / n)

  C <- sum(x * cos(alpha))
  S <- sum(x * sin(alpha))
  R <- sqrt(C*C + S*S)

  denom <- sum(abs(x))
  if (!is.finite(R) || !is.finite(denom) || denom == 0) return(NA_real_)

  r <- R / denom
  if (r <= tol_rel) return(NA_real_)

  ang <- atan2(S, C)           # [-π, π]
  if (ang < 0) ang <- ang + 2 * pi

  # Map to 1-based plotting coordinate (centres at integers 1..n), with wrap
  position <- ang / (2 * pi) * n + 1
  # Robust wrap to (0, n]
  position <- ((position - 1) %% n) + 1

  return(position)
}
