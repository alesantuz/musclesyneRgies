% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cossim.R
\name{cossim}
\alias{cossim}
\title{Cosine similarity}
\usage{
cossim(x, y)
}
\arguments{
\item{x}{A numeric vector}

\item{y}{A numeric vector}
}
\value{
The cosine similarity between two vectors
}
\description{
Cosine similarity
}
\examples{
data("act_pattern")

# Calculate the cosine similarity between an activation pattern and itself
similarity <- cossim(
x = act_pattern$signal,
y = act_pattern$signal
)

# Calculate the cosine similarity between a non-negative activation pattern and its negative
similarity <- cossim(
x = act_pattern$signal,
y = -act_pattern$signal
)
}
\references{
Leydesdorff, L. (2005)
Similarity Measures, Author Cocitation Analysis,and Information Theory.
JASIST 56(7), pp.769-772.

Fridolin Wild (2022)
lsa: Latent Semantic Analysis, R package version 0.73.3
https://CRAN.R-project.org/package=lsa
}
