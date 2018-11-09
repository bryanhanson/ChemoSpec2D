#'
#' Convert a Vector of Values to a New Scale
#'
#' This function rescales native units to a [0...1] scale, taking into
#' account that the 2D NMR plotting convention and the needs of the contour
#' function are different.
#'
#' @param vec A numeric vector of values in native units to be rescaled.
#' @param native The original data in native units.
#' @mode Integer. \code{mode == 1L} simple rescaling. \code{mode == 2L}
#'   value is subtracted from 1 (used with xlim/ylim updates).
#'
#' @export
#' @noRd
#'
.rescale <- function(vec, orig, mode = 1L) {
	out <- (vec - min(orig))/diff(range(orig))
	if (mode == 2L) out <- 1 - out # use with xlim/ylim updates
	out
}
