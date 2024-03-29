% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hurst.R
\name{Hurst}
\alias{Hurst}
\title{Hurst exponent}
\usage{
Hurst(P, min_win = 2)
}
\arguments{
\item{P}{A time series (numeric)}

\item{min_win}{Minimum window length in points}
}
\value{
A list with elements:\cr
\itemize{
\item \code{loglog} containing the log-log plot from which the HFD is calculated\cr
\item \code{Hurst} containing the Higuchi's fractal dimension of the time series.
}
}
\description{
Hurst exponent
}
\details{
\code{Hurst} calculates the Hurst exponent based on the R/S approach as in Hurst (1951).
The Hurst exponent is a measure of global complexity and it increases if the “accuracy” of
the time series decreases across several cycles (thus the term “global”).
The Hurst exponent can vary between 0 and 1. For 0.5 < Hurst exponent < 1, in the long-term high
values in the time series (the activation pattern in our case) will be probably followed by other high
values and a positive or negative trend is visible (Mandelbrot, 1983; Gneiting & Schlather, 2004).
For 0 < Hurst exponent < 0.5, in the long term high values in the series will be probably followed
by low values, with a frequent switch between high and low values
(Mandelbrot, 1983; Gneiting & Schlather, 2004). Hurst exponent = 0.5 corresponds to a completely
random series (Mandelbrot, 1983; Qian & Rasheed, 2004). In other words, values of Hurst
exponent approaching 0.5 from both ends indicate more complex (or random) behaviour of the
time series (Hurst, 1951).
For locomotor activation patterns, the minimum window length should be bigger than the period
(i.e. the length of each cycle), as reported in Santuz, Akay (2020).
}
\examples{
# Measurements of the annual flow of the river Nile at Aswan
flow <- datasets::Nile

# Calculate Hurst exponent
H <- Hurst(flow)$Hurst
message("Hurst exponent: ", round(H, 3))

# Thirty-cycle activation pattern from Santuz & Akay (2020)
data(act_pattern)
H <- Hurst(act_pattern$signal, min_win = max(act_pattern$time))$Hurst
message("Hurst exponent: ", round(H, 3))
}
\references{
Hurst, H. E.
Long-term storage capacity of reservoirs.
Trans. Am. Soc. Civ. Eng. 116, 770-808 (1951).\cr

Mandelbrot B. B.
The Fractal Geometry of Nature.
W. H. Freeman and Co., New York (1983).\cr

Gneiting T .& Schlather M.
Stochastic Models That Separate Fractal Dimension and the Hurst Effect.
SIAM Rev 46, 269–282 (2004).\cr

Qian B. & Rasheed K.
Hurst exponent and financial market predictability.
In Proceedings of the Second IASTED International Conference on Financial Engineering and
Applications, pp. 203–209 (2004).\cr

Santuz, A. & Akay, T.
Fractal analysis of muscle activity patterns during locomotion: pitfalls and how to avoid them.
J. Neurophysiol. 124, 1083-1091 (2020).
}
