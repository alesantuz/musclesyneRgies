#' Subset raw EMG
#'
#' @param x Objects of class `EMG` with elements `cycles` and `emg`
#' @param cy_max Maximum number of cycles to be considered
#' @param cy_start From which cycle should the subset begin?
#'
#' @details
#' Lists in the correct format can be created with the function `rawdata()`.
#' The first column of each `emg` element must be time in the same units as those
#' used for `cycles` (e.g., \[s\] or \[ms\]). For locomotion, thirty cycles
#' are enough for proper synergy extraction (Oliveira et al. 2014).
#'
#' @return
#' Object of class `EMG` with elements:\cr
#' - `cycles` data frame containing cycle timings, with as many columns as many cycle subdivisions are wanted\cr
#' - `emg` data frame containing raw EMG data in columns, first column is time
#'
#' @export
#'
#' @references
#' Oliveira, A. S. C., Gizzi, L., Farina, D. & Kersting, U. G.
#' Motor modules of human locomotion:
#' influence of EMG averaging, concatenation, and number of step cycles.
#' Front. Hum. Neurosci. 8, 335 (2014).
#'
#' @examples
#' # Load some data
#' data("RAW_DATA")
#' # Subset example raw data to the first 3 cycles
#' RAW_DATA_sub <- lapply(
#'   RAW_DATA,
#'   function(x) {
#'     subsetEMG(x,
#'       cy_max = 3,
#'       cy_start = 1
#'     )
#'   }
#' )
subsetEMG <- function(x,
                      cy_max,
                      cy_start = 1) {
  if (!inherits(x, "EMG")) {
    stop("Object is not of class EMG, please create objects in the right format with \"rawdata\"")
  } else {
    cycles <- x$cycles
    x <- x$emg
  }

  sub <- as.numeric(cycles[cy_start + cy_max + 1, 1])

  # Check if there are more than cy_max+2 cycles and do not trim if false
  label <- which(x[, 1] > sub)[1]

  if (!is.na(label)) {
    RAW_DATA <- list(
      cycles = cycles[cy_start:(cy_max + 1), ],
      emg = x[1:label, ]
    )
  } else {
    RAW_DATA <- list(
      cycles = cycles,
      emg = x
    )
  }

  class(RAW_DATA) <- "EMG"

  return(RAW_DATA)
}
