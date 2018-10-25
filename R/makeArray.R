### Make an Array/Data Cube from a Spectra2D Object

#'
#' @export
#' @noRd
#'

.makeArray <- function(spectra) { # stack frontal slabs with spectra$data entries
	nF1 <- length(spectra$F1)
	nF2 <- length(spectra$F2)
	nS <- length(spectra$names)
	A <- array(NA_real_,
		dim = c(nF1, nF2, nS),
		dimnames = list(rep("J", nF1), rep("I", nF2), rep("K", nS))) # rows x cols x samples
	for (k in 1:nS) A[,,k] <- spectra$data[[k]]
	return(A)
}

