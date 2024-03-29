% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normEMG.R
\name{normEMG}
\alias{normEMG}
\title{To time-normalise filtered EMG}
\usage{
normEMG(x, trim = TRUE, cy_max = NA, cycle_div = NA)
}
\arguments{
\item{x}{Object of class \code{EMG} with elements \code{cycles} and \code{emg}}

\item{trim}{Logical: should first and last cycle be trimmed to remove filtering effects?}

\item{cy_max}{Maximum number of cycles to be considered}

\item{cycle_div}{A vector or one dimensional array with the number of points each cycle should be normalised to}
}
\value{
Object of class \code{EMG} with elements:\cr
\itemize{
\item \code{cycles} data frame containing cycle timings, with as many columns as many cycle subdivisions are wanted\cr
\item \code{emg} data frame containing filtered and time-normalised EMG data in columns, first column is time
}
}
\description{
To time-normalise filtered EMG
}
\details{
Lists in the correct format can be created with the function \code{rawdata()}.
The first column of each \code{emg} element must be time in the same units as those
used for \code{cycles} (e.g., [s] or [ms]).
}
\examples{
# Load some data
data("RAW_DATA")
# Filter raw EMG
filtered_EMG <- lapply(RAW_DATA, function(x) {
  filtEMG(x, HPf = 50, HPo = 4, LPf = 20, LPo = 4)
})
# Time-normalise filtered EMG, including three cycles and trimming first and last
filt_norm_EMG <- lapply(filtered_EMG, function(x) {
  normEMG(
  x,
  cy_max = 3,
  cycle_div = c(100, 100))
})
}
\references{
Santuz, A., Ekizos, A., Janshen, L., Baltzopoulos, V. & Arampatzis, A.
On the Methodological Implications of Extracting Muscle Synergies from Human Locomotion.
Int. J. Neural Syst. 27, 1750007 (2017).
}
