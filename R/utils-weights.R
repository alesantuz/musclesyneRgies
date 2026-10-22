# After v1.4.0, muscle weights are no longer called M, but W.
# This function returns canonical muscle weights, supporting legacy objects.
.get_W <- function(x) {
  if (!is.null(x[["W"]])) {
    return(x[["W"]])
  }

  if (!is.null(x[["M"]])) {
    return(x[["M"]])
  }

  stop(
    "Object contains neither `W` nor legacy `M` muscle weights.",
    call. = FALSE
  )
}

# Ensure that both names contain identical muscle weights.
.set_W <- function(x, W) {
  x[["W"]] <- W
  x[["M"]] <- W
  x
}
