% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synsNMF.R
\name{synsNMF}
\alias{synsNMF}
\title{Non-negative matrix factorisation}
\usage{
synsNMF(
  V,
  R2_target = 0.01,
  runs = 5,
  max_iter = 1000,
  last_iter = 20,
  MSE_min = 1e-04,
  fixed_syns = NA
)
}
\arguments{
\item{V}{EMG data frame to be reconstructed, usually filtered and time-normalised}

\item{R2_target}{Threshold to stop iterations for a certain factorisation rank}

\item{runs}{Number of repetitions for each rank to avoid local minima}

\item{max_iter}{Maximum number of iterations allowed for each rank}

\item{last_iter}{How many of the last iterations should be checked before stopping?}

\item{MSE_min}{Threshold on the mean squared error to choose the factorisation rank or
minimum number of synergies}

\item{fixed_syns}{To impose the factorisation rank or number of synergies}
}
\value{
Object of class \code{musclesyneRgies} with elements:\cr
\itemize{
\item \code{syns} factorisation rank or minimum number of synergies\cr
\item \code{M} muscle weights (time-invariant coefficients)\cr
\item \code{P} activation patterns (time-dependent coefficients)\cr
\item \code{V} original data\cr
\item \code{Vr} reconstructed data\cr
\item \code{iterations} number of iterations to convergence\cr
\item \code{R2} quality of reconstruction (coefficient of determination)
\item \code{rank_type} was the rank \code{fixed} or \code{variable}?\cr
\item \code{classification} classification type (e.g., \code{none}, \code{k-means}, \code{NMF}, etc.)
}
}
\description{
Non-negative matrix factorisation
}
\details{
The first column of \code{V} must always contain time information.
}
\examples{
# Note that for bigger data sets one might want to run computation in parallel
# Load some data
data(FILT_EMG)
# Extract synergies (careful, rank is imposed here!)
SYNS <- lapply(FILT_EMG, synsNMF, fixed_syns = 4)
}
\references{
Lee, D. D. & Seung, H. S.
Learning the parts of objects by non-negative matrix factorization.
Nature 401, 788-91 (1999).\cr

Santuz, A., Ekizos, A., Janshen, L., Baltzopoulos, V. & Arampatzis, A.
On the Methodological Implications of Extracting Muscle Synergies from Human Locomotion.
Int. J. Neural Syst. 27, 1750007 (2017).\cr

Févotte, C., Idier, J.
Algorithms for Nonnegative Matrix Factorization with the Beta-Divergence
Neural Computation 23, 9 (2011).
}
