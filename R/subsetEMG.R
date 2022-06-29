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
subsetEMG <- function (x, cy_max, cy_start = 1) 
{
  if (!inherits(x, "EMG")) {
    stop("Object is not of class EMG, please create objects in the right format with \"rawdata\"")
  } else {
    cycles <- data.frame(x$cycles)
    x <- x$emg
  }
  start <- as.numeric(cycles[cy_start, 1])
  stop <- as.numeric(cycles[cy_start + cy_max, 1])
  start_emg <- tail(which(x[, 1] <= start), 1)
  stop_emg <- head(which(x[, 1] > stop), 1)
  if (length(start_emg) == 1 && length(stop_emg) == 1) {
    RAW_DATA <- list(cycles = cycles[cy_start:(cy_max + cy_start), ],
                     emg = x[start_emg:stop_emg, ])
  } else if (length(start_emg) == 1 && length(stop_emg) == 0) {
    RAW_DATA <- list(cycles = cycles[cy_start:nrow(cycles), ],
                     emg = x[start_emg:nrow(x), ])
  } else if (length(start_emg) == 0) {
    stop("Cycle times do not match time column in the EMG data!")
  }
  class(RAW_DATA) <- "EMG"
  return(RAW_DATA)
}
