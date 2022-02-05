#' Full width at half maximum
#'
#' @param x A time series (numeric)
#' @param sub_minimum Logical; should the minimum be subtracted before amplitude normalisation?
#'
#' @return The full width at half maximum of the time series.
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
#' ts <- datasets::WWWusage
#'
#' # Calculate FWHM
#' ts_FWHM <- FWHM(ts)
#'
#' # Half maximum (for the plots)
#' hm <- min(ts) + (max(ts) - min(ts)) / 2
#' hm_plot <- ts
#' hm_plot[which(hm_plot > hm)] <- hm
#' hm_plot[which(hm_plot < hm)] <- NA
#'
#' # Plots
#' plot(ts, ty = "l", xlab = "Time", ylab = "Number of users")
#' lines(hm_plot, lwd = 3, col = 2)
FWHM <- function(x, sub_minimum = TRUE) {
  if (sub_minimum) {
    # Subtract minimum
    x <- x - min(x)
  }

  # Normalise to maximum
  x <- x / max(x)

  return(length(which(x > 0.5)))
}
