#' Centre of activity
#'
#' @param x A time series  (numeric)
#'
#' @return The centre of activity of the time series, calculated with
#' circular statistics
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
CoA <- function(x) {
  points <- length(x)

  AA <- numeric()
  BB <- numeric()

  for (pp in 1:points) {
    alpha <- 360 * (pp - 1) / (points - 1) * pi / 180
    vec <- x[pp]
    AA[pp] <- vec * cos(alpha)
    BB[pp] <- vec * sin(alpha)
  }
  AA <- sum(AA)
  BB <- sum(BB)

  CoAt <- atan(BB / AA) * 180 / pi

  # To keep the sign
  if (AA > 0 && BB > 0) CoAt <- CoAt
  if (AA < 0 && BB > 0) CoAt <- CoAt + 180
  if (AA < 0 && BB < 0) CoAt <- CoAt + 180
  if (AA > 0 && BB < 0) CoAt <- CoAt + 360

  return(CoAt * points / 360)
}
