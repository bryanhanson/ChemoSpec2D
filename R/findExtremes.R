
#'
#' @export
#' @noRd
#'

.findExtreme <- function(M) {
	# Find the most extreme value in a numerical object (matrix)
	# and return the absolute value of that extreme
	# Must handle NAs
	M <- M[!is.na(M)]
	ex <- abs(range(M))
	ex <- ex[which.max(ex)]
	return(ex)
}
		
