#' Centre of activity
#'
#' @param x A time series (numeric)
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
  alpha <- seq(0, 2 * pi, length.out = length(x))

  AA <- (x * cos(alpha)) |>
    sum()
  BB <- (x * sin(alpha)) |>
    sum()

  CoAt <- atan2(BB, AA) * 180 / pi
  CoAt[CoAt < 0] <- CoAt + 360

  return(CoAt * length(x) / 360)
}
