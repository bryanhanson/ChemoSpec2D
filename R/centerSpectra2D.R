#'
#'
#' Center a Spectra2D Object Along the Samples Dimension
#' 
#' This function centers a \code{Spectra2D} object along the samples dimension.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}} from which to
#' extract peaks.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references Need that article!
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#'
#' data(MUD1)
#' tst <- centerSpectra2D(MUD1)
#'
centerSpectra2D <- function(spectra) {

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra2D(spectra)
	
	nF1 <- length(spectra$F1)
	nF2 <- length(spectra$F2)
	nS <- length(spectra$names)
	
	A1 <- array(NA_real_,
		dim = c(nF1, nF2, nS),
		dimnames = list(rep("J", nF1), rep("I", nF2), rep("K", nS))) # rows x cols x samples
	for (k in 1:nS) A1[,,k] <- spectra$data[[k]]
	
	A2 <- aperm(A1, perm = c(3, 2, 1))
	for (i in 1:nF1) A2[,,i] <- A2[,,i] - colMeans(A2[,,i])	
	
	A3 <- aperm(A2, c(3, 2, 1))
	for (k in 1:nS) {
		spectra$data[[k]] <- A3[,,k]
		dimnames(spectra$data[[k]]) <- NULL # strip dimensions from array/aperm
	}

	chkSpectra2D(spectra)		
	return(spectra)
	}
