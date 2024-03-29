% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_syn_trials.R
\name{plot_syn_trials}
\alias{plot_syn_trials}
\title{Plot muscle synergies (individual trials)}
\usage{
plot_syn_trials(
  x,
  max_syns,
  trial,
  dark_mode = FALSE,
  line_size = 0.6,
  line_col = "black",
  sd_col = "grey80",
  show_plot = TRUE
)
}
\arguments{
\item{x}{Object of class \code{musclesyneRgies}}

\item{max_syns}{Number of synergies to be plotted or how many rows should the final panel
be divided into}

\item{trial}{Character: the name of the considered trial, for archiving purposes}

\item{dark_mode}{To enable dark mode}

\item{line_size}{Line thickness}

\item{line_col}{Line colour}

\item{sd_col}{Standard deviation ribbon colour}

\item{show_plot}{Logical, to decide whether plots should be plotted in the active graphic device}
}
\value{
Plots of the unclassified synergies, trial by trial.
}
\description{
Plot muscle synergies (individual trials)
}
\details{
If \code{show_plot} is TRUE (default) plots are also shown in the active graphic device.
Plots can then be saved with the preferred export method, such as \code{ggplot2::ggsave}.
}
\examples{
# Load some data
data(SYNS)

# Find maximum number of synergies
max_syns <- max(unlist(lapply(SYNS, function(x) x$syns)))

# Save a plot with the first (and only, in this case) trial in the list
pp <- plot_syn_trials(SYNS[[1]],
  max_syns = max_syns,
  trial = names(SYNS)[1],
  dark_mode = TRUE,
  line_size = 0.8,
  line_col = "tomato1",
  sd_col = "tomato4",
  show_plot = FALSE
)
}
