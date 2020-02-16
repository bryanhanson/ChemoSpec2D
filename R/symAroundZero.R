#'
#' Symmetrize Values Around Zero
#'
#' Adjust values to be symmetric around zero so that scale is applied correctly.
#' Find the most extreme of two values, and return it and its negative.
#'
#' @export
#' @noRd
#'

.symAroundZero <- function(x) {
  if (length(x) != 2L) stop(".symAroundZero did not get two values")

  # Check for pos and neg values
  P <- N <- FALSE # flags for the existence of pos and/or neg values
  pos <- x[x > 0]
  if (length(pos) > 0) P <- TRUE
  neg <- x[x < 0]
  if (length(neg) > 0) N <- TRUE

  if ((P) & (!N)) vals <- c(-max(x), max(x))
  if ((!P) & (N)) vals <- c(min(x), -min(x))
  if ((P) & (N)) {
    mep <- max(pos) # most extreme pos value
    men <- min(neg) # most extreme neg value
    if (mep >= abs(men)) vals <- c(-mep, mep)
    if (abs(men) >= mep) vals <- c(men, -men)
  }

  return(vals)
}
