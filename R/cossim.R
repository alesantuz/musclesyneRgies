#' Cosine similarity
#'
#' @param x A numeric vector
#' @param y A numeric vector
#'
#' @return The cosine similarity between two vectors
#'
#' @export
#'
#' @references
#' Leydesdorff, L. (2005)
#' Similarity Measures, Author Cocitation Analysis,and Information Theory.
#' JASIST 56(7), pp.769-772.
#'
#' Fridolin Wild (2022)
#' lsa: Latent Semantic Analysis, R package version 0.73.3
#' https://CRAN.R-project.org/package=lsa
#'
#' @examples
#' data("act_pattern")
#'
#' # Calculate the cosine similarity between an activation pattern and itself
#' similarity <- cossim(
#' x = act_pattern$signal,
#' y = act_pattern$signal
#' )
#'
#' # Calculate the cosine similarity between a non-negative activation pattern and its negative
#' similarity <- cossim(
#' x = act_pattern$signal,
#' y = -act_pattern$signal
#' )
cossim <- function(x, y) {
    if (is.vector(x) && is.vector(y)) {
      cs <- (crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))) |>
        as.numeric()
      return(cs)
    } else {
      stop("The input must be two vectors!")
    }
}
