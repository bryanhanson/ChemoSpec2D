#'
#' Map a Vector of Values onto [0...1]
#'
#' This function rescales native units to a [0...1] scale, taking into
#' account that the 2D NMR plotting convention and the needs of the contour
#' function are different.
#'
#' @param vec A numeric vector of values in native units to be rescaled.
#'
#' @param native The original data in native units.
#'
#' @param mode Integer. \code{mode == 1L} simple rescaling. \code{mode == 2L}
#'   value is subtracted from 1 (useful for xlim/ylim updates when plotting
#'   coordinates are on [0...1] and scale has zero on the right, e.g. NMR spectra).
#'
#' @export
#' @noRd
#'
.rescale <- function(vec, orig, mode = 1L) {
	out <- (vec - min(orig))/diff(range(orig))
	if (mode == 2L) out <- 1 - out # use with xlim/ylim updates
	out
}
