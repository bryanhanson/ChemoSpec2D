### Make an Array/Data Cube from a Spectra2D Object

# Stack frontal slabs with spectra$data entries

#'
#' @export
#' @noRd
#'

.makeArray <- function(spectra) { 
	nF1 <- length(spectra$F1)
	nF2 <- length(spectra$F2)
	nS <- length(spectra$names)
	A <- array(NA_real_,
		dim = c(nF1, nF2, nS),
		dimnames = list(rep("I", nF1), rep("J", nF2), rep("K", nS))) # rows x cols x samples
	for (k in 1:nS) A[,,k] <- spectra$data[[k]]
	return(A)
}

### Make an Array/Data Cube from a Spectra2D Object

# Stack slabs horizontally (on top of each other) with *selected* spectra$data entries
# Relative to our standard data cube diagram, rotate the cube around the horizontal (x)
# axis upward, so that the front slab is on top.  1st spectrum on top.

#'
#' @export
#' @noRd
#'

.makeArray2 <- function(spectra, which = 1:3) {
	nF1 <- length(spectra$F1)
	nF2 <- length(spectra$F2)
	nS <- length(which)
	A <- array(NA_real_,
		dim = c(nS, nF1, nF2),
		dimnames = list(rep("K", nS), rep("I", nF1), rep("J", nF2))) # samples x cols x rows
	for (k in 1:nS) A[k,,] <- spectra$data[[which[k]]]
	return(A)
}
