#' To time-normalise filtered EMG
#'
#' @param x Object of class `EMG` with elements `cycles` and `emg`
#' @param trim Logical: should first and last cycle be trimmed to remove filtering effects?
#' @param cy_max Maximum number of cycles to be considered
#' @param cycle_div A vector or one dimensional array with the number of points each cycle should be normalised to
#'
#' @details
#' Lists in the correct format can be created with the function `rawdata()`.
#' The first column of each `emg` element must be time in the same units as those
#' used for `cycles` (e.g., \[s\] or \[ms\]).
#'
#' @return
#' Object of class `EMG` with elements:\cr
#' - `cycles` data frame containing cycle timings, with as many columns as many cycle subdivisions are wanted\cr
#' - `emg` data frame containing filtered and time-normalised EMG data in columns, first column is time
#'
#' @export
#'
#' @references
#' Santuz, A., Ekizos, A., Janshen, L., Baltzopoulos, V. & Arampatzis, A.
#' On the Methodological Implications of Extracting Muscle Synergies from Human Locomotion.
#' Int. J. Neural Syst. 27, 1750007 (2017).
#'
#' @examples
#' # Load some data
#' data("RAW_DATA")
#' # Filter raw EMG
#' filtered_EMG <- lapply(RAW_DATA, function(x) {
#'   filtEMG(x, HPf = 50, HPo = 4, LPf = 20, LPo = 4)
#' })
#' # Time-normalise filtered EMG, including three cycles and trimming first and last
#' norm_EMG <- lapply(filtered_EMG, function(x) {
#'   normEMG(
#'   x,
#'   cy_max = 3,
#'   cycle_div = c(100, 100))
#' })
normEMG <- function(x,
                    trim = TRUE,
                    cy_max = NA,
                    cycle_div = NA) {
  if (!inherits(x, "EMG")) {
    stop("Object is not of class EMG, please create objects in the right format with \"rawdata\"")
  } else {
    cycles <- x$cycles
    x <- x$emg
  }

  if (!identical(length(cycle_div), ncol(cycles))) {
    stop("Please check the cycle division settings")
  }

  # Save time column and remove it
  emg_time <- as.numeric(x[, 1])
  x <- x[, -1]

  if (isTRUE(trim)) {
    # Trim first and last cycle to remove filtering effects
    cycles <- cycles[2:(nrow(cycles) - 1), ]
  }
  cycs <- nrow(cycles) - 1

  # Remove excess cycles, if present
  if (!is.na(cy_max) && cycs > cy_max) cycs <- cy_max

  # Isolate cycles and normalise time to "points" points
  # (first half stance, second half swing)
  for (jj in 1:cycs) {
    for (segment in 1:length(cycle_div)) {

      # Define start of segment
      if (segment == 1) {
        t1 <- as.numeric(cycles[jj, segment])
      } else {
        t1 <- t2 + 1
      }
      # Define stop of segment
      if (segment < length(cycle_div)) {
        t2 <- as.numeric(cycles[jj, segment + 1])
      } else if (segment == length(cycle_div)) {
        t2 <- as.numeric(cycles[jj + 1, 1])
      }

      if (segment == 1) {
        t1 <- which(emg_time >= t1)[1]
      }
      t2 <- which(emg_time >= t2)[1] - 1

      # Interpolate each channel to wanted number of points
      if (jj == 1 && segment == 1) {
        emg_interp <- data.frame(
          time = c(1:cycle_div[segment]),
          apply(x[t1:t2, ], 2, function(x) {
            stats::approx(x, method = "linear", n = cycle_div[segment])$y
          })
        )
      } else {
        emg_interp <- rbind(
          emg_interp,
          data.frame(
            time = c(1:cycle_div[segment]),
            apply(x[t1:t2, ], 2, function(x) {
              stats::approx(x, method = "linear", n = cycle_div[segment])$y
            })
          )
        )
      }
    }
  }

  # Re-write time column
  emg_interp$time <- c(1:sum(cycle_div))

  return(emg_interp)
}
