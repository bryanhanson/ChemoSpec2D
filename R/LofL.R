#'
#' Create a List of Contour Levels
#'
#' When overlaying multiple 2D NMR spectra, one needs to supply a vector of contour levels for each spectrum, as a list.
#' This is a convenience function which takes a set of levels and copies it into a list, ready for
#' use  with \code{\link{plotSpectra2D}}.
#'
#' @param lvls  Numeric.  A vector of the desired levels.
#'
#' @param n Integer.  The number of spectra to be plotted, which is also the number of times to replicate the levels.
#'
#' @return A list of length \code{n}; each entry is a copy of \code{lvls}.
#'
#' @export
#'
#' @seealso \code{\link{plotSpectra2D}} for an example.
#'
LofL <- function(lvls, n) {
  ans <- list()
  for (i in 1:n) ans[[i]] <- lvls
  ans
}
