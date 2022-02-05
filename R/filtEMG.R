#' To filter raw EMG
#'
#' @param x Object of class `EMG` with elements `cycles` and `emg`
#' @param demean Logical: should EMG be demeaned?
#' @param rectif Rectification type: "fullwave", halfwave" or "none"
#' @param HPf High-pass filter cut-off frequency, use 0 to exclude high-pass filtering
#' @param HPo High-pass filter order
#' @param LPf Low-pass filter cut-off frequency, use 0 to exclude Low-pass filtering
#' @param LPo Low-pass filter order
#' @param min_sub Logical: should the minimum be subtracted?
#' @param ampl_norm Logical: should amplitude be normalised?
#'
#' @details
#' Lists in the correct format can be created with the function `rawdata()`.
#' The first column of each `emg` element must be time in the same units as those
#' used for `cycles` (e.g., \[s\] or \[ms\]).
#'
#' @return
#' Object of class `EMG` with elements:\cr
#' - `cycles` data frame containing cycle timings, with as many columns as many cycle subdivisions are wanted\cr
#' - `emg` data frame containing filtered EMG data in columns, first column is time
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
#' filtered_EMG <- pbapply::pblapply(
#'   RAW_DATA,
#'   function(x) {
#'     filtEMG(x,
#'       HPf = 50,
#'       HPo = 4,
#'       LPf = 20,
#'       LPo = 4
#'     )
#'   }
#' )
filtEMG <- function(x,
                    demean = TRUE,
                    rectif = "fullwave",
                    HPf = 50,
                    HPo = 4,
                    LPf = 20,
                    LPo = 4,
                    min_sub = TRUE,
                    ampl_norm = TRUE) {
  if (!inherits(x, "EMG")) {
    stop("Object is not of class EMG, please create objects in the right format with \"rawdata\"")
  } else {
    cycles <- x$cycles
    x <- x$emg
  }

  # Time information
  time <- x[, 1]

  # EMG system acquisition frequency [Hz]
  freq <- round(1 / (mean(diff(time), na.rm = T)), 0)

  # Remove time column
  x <- x[, -1]

  if (isTRUE(demean)) {
    x <- apply(x, 2, function(y) y - mean(y, na.rm = T))
  }

  if (HPf != 0) {
    # High-pass IIR (Infinite Impulse Response) Butterworth zero-phase filter design
    # Critical frequencies must be between 0 and 1, where 1 is the Nyquist frequency
    # "filtfilt" is for zero-phase filtering
    HPfn <- HPf / (freq / 2) # Normalise by the Nyquist frequency (f/2)
    HP <- signal::butter(HPo, HPfn, type = "high")
    x <- apply(x, 2, function(y) signal::filtfilt(HP, y))
  }

  # Rectification
  if (rectif == "fullwave") {
    x <- abs(x)
  } else if (rectif == "halfwave") {
    x[x < 0] <- 0
  }

  if (LPf != 0) {
    # Low-pass IIR (Infinite Impulse Response) Butterworth zero-phase filter design
    # Critical frequencies must be between 0 and 1, where 1 is the Nyquist frequency
    # "filtfilt" is for zero-phase filtering
    LPfn <- LPf / (freq / 2) # Normalise by the Nyquist frequency (f/2)
    LP <- signal::butter(LPo, LPfn, type = "low")
    x <- apply(x, 2, function(y) signal::filtfilt(LP, y))
  }

  # Replace values <= 0 with the smallest non-zero value
  x[x <= 0] <- min(x[x > 0], na.rm=T)

  if (isTRUE(min_sub)) {
    # Subtract the minimum
    x <- apply(x, 2, function(y) y - min(y))
  }

  if (isTRUE(ampl_norm)) {
    # Amplitude normalisation to the maximum of the trial
    x <- apply(x, 2, function(y) y / max(y))
  }

  FILT_EMG <- list(
    cycles = cycles,
    emg = cbind(time, x)
  )

  class(FILT_EMG) <- "EMG"

  return(FILT_EMG)
}
