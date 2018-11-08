#' Convert a Vector of Values to a New Scale
#'
#' @param vec A numeric vector of values in native units to be rescaled.
#' @param native The original data in native units.
#' @param new.min The minimum of the new scale.
#' @param new.max The maximum of the new scale.
#'
#' @export
#' @noRd
#'
.rescale <- function(vec, orig, new.min = 0.0, new.max = 1.0) {
	old.min = min(orig)
	old.max = max(orig)
	out <-  new.min + (new.max - new.min) * (vec - old.min)/(old.max - old.min)
	return(out)
}
