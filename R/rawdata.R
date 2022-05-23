#' Import RData or ASCII data into R
#'
#' @param path_cycles Optional, path where cycle timing files are located
#' @param path_emg Optional, path where raw EMG files are located
#' @param header_cycles Logical, are the cycle files containing a named header (the header is optional)?
#' @param header_emg Logical, are the raw EMG files containing a named header (they should)?
#'
#' @details
#' Supported are R lists saved as RData files or tab- or comma-separated files
#' readable through `read.table()` or `read.csv()`.
#' The first column of each raw emg file must be time in the same units as those used for the
#' cycle timings (e.g., \[s\] or \[ms\]).
#' If reading from RData files, please call cycles `CYCLE_TIMES.RData` and raw EMG `RAW_EMG.RData`. Lists
#' must be saved with `save()`.
#'
#' @return
#' List of objects of class `EMG`, each with elements:\cr
#' - `cycles` data frame containing cycle timings, with as many columns as many cycle subdivisions are wanted\cr
#' - `emg` data frame containing raw EMG data in columns, first column must be time in the same units as in the cycle timings
#'
#' @export
#'
#' @examples
#' # Load built-in data set
#' data("RAW_DATA")
#'
#' # Get current working directory
#' data_path <- getwd()
#' data_path <- paste0(data_path, .Platform$file.sep)
#'
#' # Create two conveniently-named subfolders if they don't already exist
#' # (if they exist, please make sure they're empty!)
#' dir.create("cycles", showWarnings = FALSE)
#' dir.create("emg", showWarnings = FALSE)
#'
#' # Export ASCII data from built-in data set to the new subfolders
#' write.table(RAW_DATA[[1]]$cycles,
#'   file = paste0(data_path, "cycles", .Platform$file.sep, names(RAW_DATA)[1], ".txt"),
#'   sep = "\t", row.names = FALSE, col.names = FALSE
#' )
#' write.table(RAW_DATA[[1]]$emg,
#'   file = paste0(data_path, "emg", .Platform$file.sep, names(RAW_DATA)[1], ".txt"),
#'   sep = "\t", row.names = FALSE
#' )
#'
#' # Run the function to parse ASCII files into objects of class `EMG`
#' raw_data_from_files <- rawdata(
#'   path_cycles = paste0(data_path, "/cycles/"),
#'   path_emg = paste0(data_path, "/emg/"),
#'   header_cycles = FALSE
#' )
#'
#' # Check data in the new folders if needed before running the following (will delete!)
#'
#' # Delete folders
#' unlink("cycles", recursive = TRUE)
#' unlink("emg", recursive = TRUE)
rawdata <- function(path_cycles = NA,
                    path_emg = NA,
                    header_cycles,
                    header_emg = TRUE) {
  if (is.na(path_cycles) && is.na(path_emg)) {
    if (.Platform$OS.type == "windows") {
      path_cycles <- utils::choose.dir(caption = "Select cycle data folder")
      path_cycles <- paste0(gsub("\\\\", .Platform$file.sep, path_cycles), .Platform$file.sep)
      path_emg <- utils::choose.dir(caption = "Select EMG data folder")
      path_emg <- paste0(gsub("\\\\", .Platform$file.sep, path_emg), .Platform$file.sep)
    } else {
      path_cycles <- tcltk::tk_choose.dir(caption = "Select cycle data folder")
      path_emg <- tcltk::tk_choose.dir(caption = "Select EMG data folder")
    }
  } else {
    path_cycles <- paste0(gsub("\\\\", .Platform$file.sep, path_cycles), .Platform$file.sep)
    path_emg <- paste0(gsub("\\\\", .Platform$file.sep, path_emg), .Platform$file.sep)
  }

  if (path_cycles == "NA/" || path_emg == "NA/") {
    stop("Please specify the folders containing raw data in RData or ASCII format")
  }

  cycle_files <- list.files(path_cycles)
  emg_files <- list.files(path_emg)
  cycle_filetype <- unique(gsub(".+\\.", "\\.", cycle_files))
  emg_filetype <- unique(gsub(".+\\.", "\\.", emg_files))

  filetype <- unique(cycle_filetype, emg_filetype)

  if (!length(filetype) == 1) {
    stop("Files must be of the same type")
  }

  files <- c(
    paste0(path_cycles, cycle_files),
    paste0(path_emg, emg_files)
  )

  RAW_DATA <- list()

  # Read cycle times
  files <- cycle_files
  data_path <- path_cycles

  if (filetype == ".txt" || filetype == ".csv") {
    for (file in files) {
      trial <- gsub(filetype, "", file)
      if (filetype == ".txt") {
        RAW_DATA[[trial]]$cycles <- utils::read.table(paste0(data_path, file), header_cycles)
      } else if (filetype == ".csv") {
        RAW_DATA[[trial]]$cycles <- utils::read.csv(paste0(data_path, file), header_cycles)
      }
    }
  } else if (filetype == ".RData") {
    CYCLE_TIMES <- NULL
    RAW_EMG <- NULL
    load(paste0(data_path, "CYCLE_TIMES.RData"))
  }

  # Read raw EMG
  files <- emg_files
  data_path <- path_emg

  if (filetype == ".txt" || filetype == ".csv") {
    for (file in files) {
      trial <- gsub(filetype, "", file)
      if (filetype == ".txt") {
        RAW_DATA[[trial]]$emg <- utils::read.table(paste0(data_path, file), header_emg)
      } else if (filetype == ".csv") {
        RAW_DATA[[trial]]$emg <- utils::read.csv(paste0(data_path, file), header_emg)
      }
      class(RAW_DATA[[trial]]) <- "EMG"
    }
  } else if (filetype == ".RData") {
    load(paste0(data_path, "RAW_EMG.RData"))
    for (trial in names(CYCLE_TIMES)) {
      RAW_DATA[[trial]]$cycles <- CYCLE_TIMES[[trial]]
      RAW_DATA[[trial]]$emg <- RAW_EMG[[trial]]
      class(RAW_DATA[[trial]]) <- "EMG"
    }
  }
  return(RAW_DATA)
}
