#'
#'
#' Center and Optionally Scale a Spectra2D Object Along the Samples Dimension
#' 
#' This function will center, and optionally scale, a \code{Spectra2D} object along the
#' samples dimension (i.e. this is pixel-wise scaling in the language of multivariate
#' image analysis). Several scaling options are available.
#' The data is centered before scaling is applied.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param choice A character string indicating the type of scaling to apply.  One of
#' \code{c("autoscale", "Pareto")}.  
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references
#' R. Bro and A. K. Smilde "Centering and Scaling in Component Analysis" J. Chemometrics
#' vol. 17 pgs 16-33 (2003).
#'
#' @keywords utilities
#'
#' @export
#'
#'
#' @examples
#'
#' data(MUD1)
#' tst <- centscaleSpectra2D(MUD1)
#'
centscaleSpectra2D <- function(spectra, choice = "noscale") {

	.chkArgs(mode = 21L)
	chkSpectra(spectra)

  if (!requireNamespace("matrixStats", quietly = TRUE)) {
    stop("You must install package matrixStats to use this function")
  }
	
	A1 <- .makeArray(spectra) # frontal slabs contain spectra$data entries
	A2 <- aperm(A1, perm = c(3, 2, 1))
	
	# center the data
	for (i in 1:length(spectra$F1)) A2[,,i] <- A2[,,i] - matrixStats::colMeans2(A2[,,i], na.rm = TRUE)
	
	if (choice == "autoscale") {
		for (i in 1:length(spectra$F1)) A2[,,i] <- A2[,,i]/matrixStats::colSds(A2[,,i], na.rm = TRUE)	
	}
	
	if (choice == "Pareto") {
		for (i in 1:length(spectra$F1)) A2[,,i] <- A2[,,i]/sqrt(matrixStats::colSds(A2[,,i], na.rm = TRUE))	
	}
		
	A3 <- aperm(A2, c(3, 2, 1)) # restore original orientation
	for (k in 1:length(spectra$names)) {
		spectra$data[[k]] <- A3[,,k]
		dimnames(spectra$data[[k]]) <- NULL # strip dimensions from array/aperm
	}

	chkSpectra(spectra)		
	return(spectra)
	}
