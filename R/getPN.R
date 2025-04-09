#'
#' Get the Positive or Negative Values in a Matrix
#'
#' @noRd
#'

.getPN <- function(M) {
  # Get either the (+)-ive or (-)-ive values in a matrix
  # depending upon which is most extreme in absolute terms,
  # and return them as a vector of positive values

  # Be sure to weed out NA's

  neg <- M[M < 0] # these are vectors
  neg <- neg[!is.na(neg)]
  pos <- M[M > 0]
  pos <- pos[!is.na(pos)]
  if (length(pos) == 0) {
    return(neg)
  } # no + values
  if (length(neg) == 0) {
    return(pos)
  } # no - values
  exP <- .findExtreme(pos)
  exN <- .findExtreme(neg)
  if (exP >= exN) {
    return(pos)
  }
  if (exN > exP) {
    return(abs(neg))
  }
}
